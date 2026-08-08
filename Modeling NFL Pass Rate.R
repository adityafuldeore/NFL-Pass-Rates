#PACKAGES
library(nflfastR)   
library(nflreadr)   
library(dplyr)
library(tidyr)
library(glmnet)     
library(Matrix)     
library(class)      
library(ROCR)     
library(tidyverse)

set.seed(2026)

# DATA LOADING ---------------------------------------------

pbp_eda <- load_pbp(2022:2025)

colnames(pbp_eda)

#get regulars season runs/passess
pbp_raw <- load_pbp(2022:2025) %>%
  filter(
    season_type  == "REG",           
    play_type    %in% c("run","pass"),
    !is.na(down),
    !is.na(ydstogo),
    !is.na(yardline_100),
    !is.na(score_differential),
    !is.na(wp),
    !is.na(game_seconds_remaining)
  ) %>%
  dplyr::select(
    # Identifiers 
    game_id, play_id, posteam, defteam, season, week, away_team, home_team,
    
    # Indep variable
    play_type,
    
    # Down / distance 
    down,              
    ydstogo,            
    goal_to_go,         
    
    # Field position
    yardline_100,       
    
    # Game state
    score_differential,       
    wp,                      
    game_seconds_remaining,    
    half_seconds_remaining,     
    qtr,                  
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    
    # Formation
    shotgun,  #1 if QB in shotgun
    no_huddle,    #1 if no-huddle offense
    
    # Environment
    roof,             
    temp,               
    wind,               
    surface             
  )

dim(pbp_raw)

#Load personnel participation

participation <- load_participation(seasons = 2022:2025, include_pbp = FALSE) %>%
  dplyr::select(
    possession_team,
    nflverse_game_id,
    play_id,
    offense_formation,  
    offense_personnel,  
    defenders_in_box
  ) %>%
  rename(game_id = nflverse_game_id)

#Parse 
participation <- participation %>%
  mutate(
    #Extract number of RBs, TEs, WRs from the personnel string
    n_rb = as.integer(gsub(".*?(\\d+) RB.*", "\\1", offense_personnel)),
    n_te = as.integer(gsub(".*?(\\d+) TE.*", "\\1", offense_personnel)),
    n_wr = as.integer(gsub(".*?(\\d+) WR.*", "\\1", offense_personnel)),
    #personnel groupings as binary flags
    personnel_11 = as.integer(n_rb == 1 & n_te == 1 & n_wr == 3),  # 3-WR set
    personnel_12 = as.integer(n_rb == 1 & n_te == 2 & n_wr == 2),  # 2-TE set
    personnel_21 = as.integer(n_rb == 2 & n_te == 1 & n_wr == 2),  # 2-RB set
    personnel_22 = as.integer(n_rb == 2 & n_te == 2 & n_wr == 1),  # heavy set
    
    n_rb = ifelse(is.na(n_rb), 0L, n_rb),
    n_te = ifelse(is.na(n_te), 0L, n_te),
    n_wr = ifelse(is.na(n_wr), 0L, n_wr)
  )


#JOIN & BUILD MASTER DATASET ---------------------------------------

pbp <- pbp_raw %>%
  left_join(participation, by = c("game_id","play_id")) %>%
  mutate(
    is_pass = as.integer(play_type == "pass"),
    #Impute
    is_dome  = as.integer(roof %in% c("dome","closed")),
    temp  = ifelse(is_dome == 1 | is.na(temp), 65, temp),
    wind  = ifelse(is_dome == 1 | is.na(wind),  0, wind),
    #Impute missing personnel
    defenders_in_box = ifelse(is.na(defenders_in_box), 6L, defenders_in_box),
    personnel_11  = ifelse(is.na(personnel_11), 0L, personnel_11),
    personnel_12  = ifelse(is.na(personnel_12), 0L, personnel_12),
    personnel_21  = ifelse(is.na(personnel_21), 0L, personnel_21),
    personnel_22  = ifelse(is.na(personnel_22), 0L, personnel_22),
    n_wr = ifelse(is.na(n_wr), 0L, n_wr),
    n_rb  = ifelse(is.na(n_rb), 0L, n_rb),
    n_te  = ifelse(is.na(n_te), 0L, n_te)
  ) %>%
  filter(
    !is.na(shotgun), !is.na(no_huddle)
  )


nrow(pbp)
sum(pbp$is_pass)
sum(pbp$is_pass == 0)
round(mean(pbp$is_pass)*100, 1)

boxplot(pbp$is_pass)

#EDA
play_counts <- pbp %>%
  mutate(play_type = ifelse(is_pass == 1, "Pass", "Run")) %>%
  group_by(play_type) %>%
  summarise(n_plays = n(), .groups = "drop") %>%
  mutate(
    pct = round(n_plays / sum(n_plays) * 100, 1),
  )

ggplot(play_counts, aes(x = play_type, y = n_plays, fill = play_type)) +
  geom_col(width = 0.4) + 
  scale_fill_manual(
    values = c("Pass" = "steelblue", "Run" = "red"),
    guide  = "none"
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Run vs Pass Play Distribution",
    x = "Play Type",
    y = "Number of Plays"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey40", size = 10),
  )


#EXPLORATORY DATA ANALYSIS

pbp %>%
  group_by(down) %>%
  summarise(n_plays = n(), pass_rate = round(mean(is_pass), 3), .groups="drop") %>%
  ggplot(aes(x = factor(down), y = pass_rate, fill = factor(down))) +
  geom_col(width = 0.5) +
  labs(
    title = "Pass Rate by Down",
    subtitle = "NFL Regular Season, 2022-2025",
    x = "Down",
    y = "Pass Rate"
  ) + theme_minimal()

pbp %>%
  mutate(dist_bucket = cut(ydstogo,
                           breaks = c(0,3,6,10,Inf),
                           labels = c("Short(1-3)","Med(4-6)","Long(7-10)","XLong(11+)"))) %>%
  group_by(down, dist_bucket) %>%
  summarise(pass_rate = round(mean(is_pass), 3), n = n(), .groups="drop") %>%
  pivot_wider(names_from = dist_bucket, values_from = pass_rate) %>%
  data.frame() %>% write.csv("down_dist.csv")


pbp %>%
  group_by(shotgun) %>%
  summarise(n = n(), pass_rate = round(mean(is_pass), 3), .groups="drop") %>%
  mutate(formation = ifelse(shotgun==1,"Shotgun","Under Center")) %>%
  select(formation, n, pass_rate) %>% 
  ggplot(aes(x = formation, y = pass_rate, fill = formation)) +
  geom_col(width = 0.5) +
  labs(
    title = "Pass Rate by Formation",
    subtitle = "NFL Regular Season, 2022-2025",
    x = "Down",
    y = "Pass Rate"
  ) + theme_minimal()


cat("\n--- 4f. Pass Rate by Personnel Grouping ---\n")
pbp %>%
  mutate(personnel = case_when(
    personnel_11 == 1 ~ "11 (1RB-1TE-3WR)",
    personnel_12 == 1 ~ "12 (1RB-2TE-2WR)",
    personnel_21 == 1 ~ "21 (2RB-1TE-2WR)",
    personnel_22 == 1 ~ "22 (2RB-2TE-1WR)",
    TRUE               ~ "Other"
  )) %>%
  group_by(personnel) %>%
  summarise(n = n(), pass_rate = round(mean(is_pass), 3), .groups="drop") %>%
  arrange(desc(n)) %>%
  write.csv('personnel_pass_rt.csv')


pbp %>%
  group_by(posteam) %>%
  summarise(n = n(), pass_rate = round(mean(is_pass),3), .groups="drop") %>%
  arrange(desc(pass_rate)) %>%
  write.csv("team_pass_rate.csv")


num_feats <- c("down","ydstogo","yardline_100","score_differential",
               "wp","game_seconds_remaining", 
               "temp","wind","defenders_in_box")
pbp %>%
  select(all_of(num_feats)) %>%
  summarise(across(everything(), list(
    mean = ~round(mean(., na.rm=TRUE),2),
    sd   = ~round(sd(.,   na.rm=TRUE),2),
    min  = ~round(min(.,  na.rm=TRUE),2),
    max  = ~round(max(.,  na.rm=TRUE),2)
  ))) %>%
  pivot_longer(everything(), names_to="stat", values_to="value") %>%
  separate(stat, into=c("feature","metric"), sep="_(?=[^_]+$)") %>%
  pivot_wider(names_from=metric, values_from=value) %>%
  write.csv('num_feats.csv')


# CHECK CORRELATIONS

library(corrplot)

cont_var <- c(
  "ydstogo", "yardline_100", "score_differential",
  "wp", "game_seconds_remaining", "half_seconds_remaining",
  "spread_line", "total_line", "temp", "wind", "defenders_in_box"
)

colnames(pbp)
numeric_var <- pbp[sapply(pbp, is.numeric)]
cor_matrix <- cor(numeric_var, use = "complete.obs")
# cor_pairs <- which(abs(cor_matrix) > 0.75 & upper.tri(cor_matrix), arr.ind = TRUE)
cor_pairs
if (nrow(cor_pairs) == 0) {
  cat("No pairs exceed threshold.\n")
} else {
  cor_flags <- data.frame(
    feature_1   = rownames(cor_matrix)[cor_pairs[, 1]],
    feature_2   = colnames(cor_matrix)[cor_pairs[, 2]],
    correlation = round(cor_matrix[cor_pairs], 3)
  ) %>% arrange(desc(abs(correlation)))
  print(cor_flags, row.names = FALSE)
}
write_csv(data.frame(cor_matrix), 'cor_mat.csv')

high_cor <- c('game_seconds_remaining', 'n_wr', 'wp')

#FEATURE ENGINEERING ---------------------------------

pbp <- pbp %>%
  mutate(
    redzone = as.integer(yardline_100 <= 20),
    backed_up   = as.integer(yardline_100 >= 85),
    two_min_drill  = as.integer(half_seconds_remaining < 120),
    trailing_large = as.integer(score_differential < -14),
    leading_large  = as.integer(score_differential > 14),
    late_game      = as.integer(qtr >= 4),
    
    down_x_ydstogo  = down * ydstogo
    
  ) 

#TRAIN / TEST SPLIT & SCALING

feature_cols <- c(
  
  "down", "ydstogo", "yardline_100", "score_differential",
  "wp", "vegas_wp", "game_seconds_remaining", "half_seconds_remaining",
  "spread_line", "total_line", "shotgun", "no_huddle",
  "posteam_timeouts_remaining", "defteam_timeouts_remaining",
  "goal_to_go", "is_dome", "temp", "wind",
  
  "redzone", "backed_up", "two_min_drill",
  "trailing_large", "leading_large", "late_game", "high_wind",
  
  "personnel_11", "personnel_12", "personnel_21", "personnel_22",
  "n_rb", "n_te", "n_wr", "defenders_in_box",
  
  "down_x_ydstogo"
  
)

# Keep only feature columns that exist and have no NAs
feature_cols <- feature_cols[feature_cols %in% names(pbp)]

feature_cols <- feature_cols[!feature_cols %in% high_cor] #not highly correlated

pbp_model <- pbp %>% select(all_of(feature_cols), is_pass, season, posteam) %>% drop_na()


train <- pbp_model %>% filter(season <= 2024 & season >= 2022)
test  <- pbp_model %>% filter(season == 2025)

nrow(train)
nrow(test)
length(feature_cols)
round(mean(train$is_pass),3)
round(mean(test$is_pass),3)

X_train <- as.matrix(train[,feature_cols])
y_train <- train$is_pass
X_test <- as.matrix(test[,feature_cols])
y_test <- test$is_pass

#Standardize
tr_means <- colMeans(X_train)
tr_sds  <- apply(X_train, 2, sd)
tr_sds[tr_sds == 0] <- 1  
X_train_sc <- scale(X_train, center = tr_means, scale = tr_sds)
X_test_sc  <- scale(X_test,  center = tr_means, scale = tr_sds)

# SECTION 7: EVALUATION HELPER
eval_help <- function(probs, labels, threshold = 0.5, model_name = "") {
  preds <- as.integer(probs >= threshold)
  acc <- mean(preds == labels)
  pred_obj <- prediction(probs, labels)
  auc_val <- as.numeric(performance(pred_obj, "auc")@y.values)
  tp <- sum(preds == 1 & labels == 1)
  tn <- sum(preds == 0 & labels == 0)
  fp <- sum(preds == 1 & labels == 0)
  fn <- sum(preds == 0 & labels == 1)
  sen <- tp / (tp + fn)
  spec <- tn / (tn + fp)
  if (nchar(model_name) > 0) {
    cat("Accuracy :", round(acc,  4), "\n")
    cat("AUC :", round(auc_val, 4), "\n")
    cat("Sensitivity:", round(sen, 4), "\n") #pass recall
    cat("Specificity:", round(spec, 4), "\n") #run recall
  }
  list(accuracy=round(acc,4), auc=round(auc_val,4),
       sensitivity=round(sen,4), specificity=round(spec,4))
}

#BASELINE — LOGISTIC REGRESSION
logit_fit <- glm(is_pass ~ .,
                 data   = cbind(as.data.frame(X_train_sc), is_pass = y_train),
                 family = binomial
)
logit_probs <- predict(logit_fit, newdata=as.data.frame(X_test_sc), type="response")
log_eval <- eval_help(logit_probs, y_test, model_name="Logistic")

# Coefficients sorted by magnitude
logit_coefs <- coef(logit_fit)[-1]
logit_coef_df <- data.frame(
  feature = names(logit_coefs),
  coef    = round(logit_coefs, 4)
) %>% arrange(desc(abs(coef)))

cat("\nLogistic Coefficients (standardized, sorted by |magnitude|):\n")
print(logit_coef_df, row.names = FALSE)
summary(logit_fit)

#RIDGE LOGISTIC REGRESSION
set.seed(42)
cv_ridge <- cv.glmnet(X_train_sc, y_train, alpha=0,
                      family="binomial", nfolds=5, type.measure="auc")
round(cv_ridge$lambda.min, 5)

plot(
  cv_ridge$lambda, cv_ridge$cvm,
  type  = "l",
  col   = "steelblue",
  lwd   = 2,
  xlab  = "Lambda", ylab  = "Mean CV AUC",
  main  = "Ridge Regression: Optimal Lambda"
)
abline(v = cv_ridge$lambda.min, col = "red", lty = 2)

ridge_fit <- glmnet(X_train_sc, y_train, alpha=0,
                    family="binomial", lambda=cv_ridge$lambda.min)
ridge_probs <- as.numeric(predict(ridge_fit, newx=X_test_sc, type="response"))
rg_eval <- eval_help(ridge_probs, y_test, model_name="Ridge")

ridge_coefs <- as.numeric(coef(ridge_fit))[-1]
names(ridge_coefs) <- feature_cols
ridge_coef_df <- data.frame(
  feature = names(ridge_coefs),
  coef    = round(ridge_coefs, 4)
) %>% arrange(desc(abs(coef)))

print(ridge_coef_df, row.names = FALSE)
summary(ridge_fit)

#LASSO LOGISTIC REGRESSION

set.seed(42)
cv_lasso <- cv.glmnet(X_train_sc, y_train, alpha=1,
                      family="binomial", nfolds=5, type.measure="auc")
cat("Lambda.min:", round(cv_lasso$lambda.min, 5))

lasso_fit   <- glmnet(X_train_sc, y_train, alpha=1,
                      family="binomial", lambda=cv_lasso$lambda.min)
lasso_probs <- as.numeric(predict(lasso_fit, newx=X_test_sc, type="response"))
ls_eval <- eval_help(lasso_probs, y_test, model_name="Lasso")

lasso_coefs  <- as.numeric(coef(lasso_fit))[-1]
names(lasso_coefs) <- feature_cols
retained_features  <- names(lasso_coefs[lasso_coefs != 0])
zeroed_features <- names(lasso_coefs[lasso_coefs == 0])

lasso_coef_df <- data.frame(
  feature = retained_features,
  coef    = round(lasso_coefs[retained_features], 4)
) %>% arrange(desc(abs(coef)))


print(lasso_coef_df, row.names = FALSE)


#K-NEAREST NEIGHBORS

set.seed(42)
val_idx  <- sample(seq_len(nrow(X_train_sc)), size=floor(0.10*nrow(X_train_sc)))
X_tr_sub <- X_train_sc[-val_idx, ]
y_tr_sub <- y_train[-val_idx]
X_val    <- X_train_sc[val_idx, ]
y_val    <- y_train[val_idx]

k_grid  <- c(5, 25, 50, 100, 200)
k_results <- data.frame(k=k_grid, val_accuracy=NA, val_auc=NA)

for (i in seq_along(k_grid)) {
  knn_val <- knn(train=X_tr_sub, test=X_val, cl=y_tr_sub,
                 k=k_grid[i], prob=TRUE)
  prob_attr  <- attr(knn_val, "prob")
  pred_class <- as.integer(as.character(knn_val))
  prob_pass  <- ifelse(pred_class==1, prob_attr, 1-prob_attr)
  k_results$val_accuracy[i] <- round(mean(pred_class==y_val), 4)
  k_results$val_auc[i]  <- round(
    as.numeric(performance(prediction(prob_pass, y_val),"auc")@y.values), 4)
}
print(k_results, row.names=FALSE)

best_k <- k_results$k[which.max(k_results$val_auc)]

#plot knn errors
plot(k_grid, k_results$val_auc,
     type = "b", pch = 16, col = "steelblue", 
     xlab = "k", ylab = "AUC")

# Refit on full training set
knn_final <- knn(train=X_train_sc, test=X_test_sc,
                 cl=y_train, k=best_k, prob=TRUE)
prob_attr_te  <- attr(knn_final, "prob")
pred_class_te <- as.integer(as.character(knn_final))
knn_probs <- ifelse(pred_class_te==1, prob_attr_te, 1-prob_attr_te)

knn_eval <- eval_help(knn_probs, y_test, model_name="KNN")

#PRINCIPAL COMPONENT REGRESSION

#Feature consensus set
n_ridge_top <- min(15, length(ridge_coefs))
ridge_top   <- ridge_coef_df$feature[1:n_ridge_top]
pcr_feature_set <- union(retained_features, ridge_top)

X_train_pcr <- X_train_sc[, pcr_feature_set]
X_test_pcr  <- X_test_sc[,  pcr_feature_set]

#PCA
pca_fit   <- prcomp(X_train_pcr, center=FALSE, scale.=FALSE)
var_exp <- pca_fit$sdev^2 / sum(pca_fit$sdev^2)
cum_var <- cumsum(var_exp)

cat("Variance explained per component:\n")
pca_summary <- data.frame(
  component = paste0("PC", seq_along(var_exp)),
  pct_var   = round(var_exp * 100, 2),
  cum_pct   = round(cum_var * 100, 2)
)
print(pca_summary, row.names=FALSE)

n_show <- min(3, ncol(pca_fit$rotation))
rot_mat <- pca_fit$rotation[, 1:n_show, drop=FALSE]
loadings_df <- data.frame(feature=pcr_feature_set, round(rot_mat, 3),
                          stringsAsFactors=FALSE)
loadings_df <- loadings_df[order(abs(loadings_df$PC1), decreasing=TRUE), ]
print(loadings_df, row.names=FALSE)

#5-fold CV over number of components
set.seed(42)
n_folds <- 5
fold_id  <- sample(rep(1:n_folds, length.out=nrow(X_train_pcr)))
max_comp <- length(pcr_feature_set)

train_scores_full <- predict(pca_fit, X_train_pcr)
pcr_cv <- data.frame(n_comp=1:max_comp, cv_auc=NA)

for (nc in 1:max_comp) {
  fold_aucs <- numeric(n_folds)
  for (fold in 1:n_folds) {
    tr_idx   <- fold_id != fold
    val_idx2 <- fold_id == fold
    pc_tr    <- train_scores_full[tr_idx,  1:nc, drop=FALSE]
    pc_val   <- train_scores_full[val_idx2, 1:nc, drop=FALSE]
    y_tr2    <- y_train[tr_idx]
    y_val2   <- y_train[val_idx2]
    fit_cv   <- suppressWarnings(
      glm(y_tr2 ~ ., data=data.frame(pc_tr), family=binomial))
    p_val <- predict(fit_cv, newdata=data.frame(pc_val), type="response")
    fold_aucs[fold] <- as.numeric(
      performance(prediction(p_val, y_val2),"auc")@y.values)
  }
  pcr_cv$cv_auc[nc] <- round(mean(fold_aucs), 4)
}

best_ncomp <- 12

plot(
  pcr_cv$n_comp, pcr_cv$cv_auc,
  type = "l",col  = "steelblue",
  xlab = "Number of Components",
  ylab = "CV AUC",
  main = "PCR: Number of Components vs CV AUC"
)
abline(v = 12, col = "red", lty = 2)

#Final PCR model
pc_train_final <- train_scores_full[, 1:best_ncomp, drop=FALSE]
test_scores <- predict(pca_fit, X_test_pcr)
pc_test_final  <- test_scores[, 1:best_ncomp, drop=FALSE]

pcr_fit <- glm(y_train ~ ., data=data.frame(pc_train_final), family=binomial)
pcr_probs <- predict(pcr_fit, newdata=data.frame(pc_test_final), type="response")

pc_eval <- eval_help(pcr_probs, y_test, model_name="PCR")


#FULL MODEL COMPARISON

comparison <- data.frame(
  Model = c(
    "Logistic (Baseline)", "Ridge Logistic", "Lasso Logistic",
    paste0("KNN (k=", best_k, ")"),
    paste0("PCR (", best_ncomp, " components)")
  ),
  Accuracy    = c(log_eval$accuracy, rg_eval$accuracy, ls_eval$accuracy, knn_eval$accuracy, pc_eval$accuracy),
  AUC  = c(log_eval$auc,      rg_eval$auc,      ls_eval$auc,      knn_eval$auc,      pc_eval$auc),
  Sensitivity = c(log_eval$sensitivity, rg_eval$sensitivity, ls_eval$sensitivity,
                  knn_eval$sensitivity, pc_eval$sensitivity),
  Specificity = c(log_eval$specificity, rg_eval$specificity, ls_eval$specificity,
                  knn_eval$specificity, pc_eval$specificity)
) %>%
  mutate(
    diff_AUC = round(AUC - AUC[1], 4),
    diff_Acc = round((Accuracy - Accuracy[1]) * 100, 2)
  )

print(comparison, row.names=FALSE)

ggplot(comparison, aes(x = Model, y = AUC, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 4) +
  geom_text(aes(label = round(AUC, 4)),
            vjust = -1.0, fontface = "bold", size = 3.8) +
  scale_y_continuous(limits = c(min(comparison$AUC) - 0.01,
                                max(comparison$AUC) + 0.02)) +
  labs(title = "Model Comparison: AUC",
       x= NULL,
       y  = "AUC") +
  theme_minimal(base_size = 13) 


#TEAM PREDICTABILITY RANKING

teams <- unique(test$posteam)

team_pred_knn <- lapply(teams, function(tm) {
  tm_idx    <- test$posteam == tm
  X_tm  <- X_test_sc[tm_idx, , drop = FALSE]
  y_tm  <- y_test[tm_idx]
  
  knn_tm  <- knn(train = X_train_sc, test = X_tm,
                 cl = y_train, k = best_k, prob = TRUE)
  prob_attr   <- attr(knn_tm, "prob")
  pred_class  <- as.integer(as.character(knn_tm))
  probs_tm  <- ifelse(pred_class == 1, prob_attr, 1 - prob_attr)
  
  auc_tm <-  as.numeric(performance(prediction(probs_tm, y_tm), "auc")@y.values)
  
  
  data.frame(
    team   = tm,
    n_plays = sum(tm_idx),
    actual_pass_rate = round(mean(y_tm), 3),
    knn_accuracy     = round(mean(pred_class == y_tm), 3),
    knn_auc = round(auc_tm, 3)
  )
}) %>%
  bind_rows() %>%
  arrange(desc(knn_accuracy))


print(team_pred_knn, row.names = FALSE)
write.csv(team_pred, 'team_pred.csv')


ridge_coef_df
lasso_coef_df
summary(pcr_fit)


