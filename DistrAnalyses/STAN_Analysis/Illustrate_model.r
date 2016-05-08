model <- read.csv("fixEf_Transf3_output.csv", header = T)
model.graph = data.frame(Age = rep(rep(c("Adult","5-yr-old","3-yr-old"), each = 4), times = 3),
                         AnswerType = rep(rep(c("Predictable","Unpredictable"), each = 2),times = 9),
                         SceneType = rep(c("Match","Mismatch"), times = 18),
                         Parameter = rep(c("Mu", "Sigma","Tau"), each = 12))
model.graph$Value = NA

model.graph[model.graph$Parameter == "Mu" & model.graph$Age == "Adult" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Match",]$Value <- subset(model, X == "beta[1]")$mean
model.graph[model.graph$Parameter == "Mu" & model.graph$Age == "Adult" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta[1]")$mean + subset(model, X == "beta[2]")$mean)
model.graph[model.graph$Parameter == "Mu" & model.graph$Age == "Adult" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Match",]$Value <- (subset(model, X == "beta[1]")$mean + subset(model, X == "beta[3]")$mean)
model.graph[model.graph$Parameter == "Mu" & model.graph$Age == "Adult" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta[1]")$mean + subset(model, X == "beta[2]")$mean + subset(model, X == "beta[3]")$mean + subset(model, X == "beta[6]")$mean)

model.graph[model.graph$Parameter == "Mu" & model.graph$Age == "5-yr-old" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Match",]$Value <- (subset(model, X == "beta[1]")$mean + subset(model, X == "beta[4]")$mean)
model.graph[model.graph$Parameter == "Mu" & model.graph$Age == "5-yr-old" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta[1]")$mean + subset(model, X == "beta[4]")$mean + subset(model, X == "beta[2]")$mean  + subset(model, X == "beta[7]")$mean)
model.graph[model.graph$Parameter == "Mu" & model.graph$Age == "5-yr-old" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Match",]$Value <- (subset(model, X == "beta[1]")$mean + subset(model, X == "beta[4]")$mean + subset(model, X == "beta[3]")$mean+ subset(model, X == "beta[9]")$mean)
model.graph[model.graph$Parameter == "Mu" & model.graph$Age == "5-yr-old" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta[1]")$mean + subset(model, X == "beta[2]")$mean+ subset(model, X == "beta[4]")$mean  + subset(model, X == "beta[3]")$mean + subset(model, X == "beta[6]")$mean+ subset(model, X == "beta[7]")$mean+ subset(model, X == "beta[9]")$mean   + subset(model, X == "beta[11]")$mean)

model.graph[model.graph$Parameter == "Mu" & model.graph$Age == "3-yr-old" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Match",]$Value <- (subset(model, X == "beta[1]")$mean + subset(model, X == "beta[5]")$mean)
model.graph[model.graph$Parameter == "Mu" & model.graph$Age == "3-yr-old" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta[1]")$mean + subset(model, X == "beta[5]")$mean + subset(model, X == "beta[2]")$mean  + subset(model, X == "beta[8]")$mean)
model.graph[model.graph$Parameter == "Mu" & model.graph$Age == "3-yr-old" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Match",]$Value <- (subset(model, X == "beta[1]")$mean + subset(model, X == "beta[5]")$mean + subset(model, X == "beta[3]")$mean+ subset(model, X == "beta[10]")$mean)
model.graph[model.graph$Parameter == "Mu" & model.graph$Age == "3-yr-old" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta[1]")$mean + subset(model, X == "beta[2]")$mean+ subset(model, X == "beta[5]")$mean  + subset(model, X == "beta[3]")$mean + subset(model, X == "beta[6]")$mean+ subset(model, X == "beta[8]")$mean+ subset(model, X == "beta[10]")$mean   + subset(model, X == "beta[12]")$mean)

######## Sigma
model.graph[model.graph$Parameter == "Sigma" & model.graph$Age == "Adult" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Match",]$Value <- subset(model, X == "beta_s[1]")$mean
model.graph[model.graph$Parameter == "Sigma" & model.graph$Age == "Adult" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta_s[1]")$mean + subset(model, X == "beta_s[2]")$mean)
model.graph[model.graph$Parameter == "Sigma" & model.graph$Age == "Adult" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Match",]$Value <- (subset(model, X == "beta_s[1]")$mean + subset(model, X == "beta_s[3]")$mean)
model.graph[model.graph$Parameter == "Sigma" & model.graph$Age == "Adult" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta_s[1]")$mean + subset(model, X == "beta_s[2]")$mean + subset(model, X == "beta_s[3]")$mean + subset(model, X == "beta_s[6]")$mean)

model.graph[model.graph$Parameter == "Sigma" & model.graph$Age == "5-yr-old" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Match",]$Value <- (subset(model, X == "beta_s[1]")$mean + subset(model, X == "beta_s[4]")$mean)
model.graph[model.graph$Parameter == "Sigma" & model.graph$Age == "5-yr-old" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta_s[1]")$mean + subset(model, X == "beta_s[4]")$mean + subset(model, X == "beta_s[2]")$mean  + subset(model, X == "beta_s[7]")$mean)
model.graph[model.graph$Parameter == "Sigma" & model.graph$Age == "5-yr-old" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Match",]$Value <- (subset(model, X == "beta_s[1]")$mean + subset(model, X == "beta_s[4]")$mean + subset(model, X == "beta_s[3]")$mean+ subset(model, X == "beta_s[9]")$mean)
model.graph[model.graph$Parameter == "Sigma" & model.graph$Age == "5-yr-old" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta_s[1]")$mean + subset(model, X == "beta_s[2]")$mean+ subset(model, X == "beta_s[4]")$mean  + subset(model, X == "beta_s[3]")$mean + subset(model, X == "beta_s[6]")$mean+ subset(model, X == "beta_s[7]")$mean+ subset(model, X == "beta_s[9]")$mean   + subset(model, X == "beta_s[11]")$mean)

model.graph[model.graph$Parameter == "Sigma" & model.graph$Age == "3-yr-old" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Match",]$Value <- (subset(model, X == "beta_s[1]")$mean + subset(model, X == "beta_s[5]")$mean)
model.graph[model.graph$Parameter == "Sigma" & model.graph$Age == "3-yr-old" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta_s[1]")$mean + subset(model, X == "beta_s[5]")$mean + subset(model, X == "beta_s[2]")$mean  + subset(model, X == "beta_s[8]")$mean)
model.graph[model.graph$Parameter == "Sigma" & model.graph$Age == "3-yr-old" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Match",]$Value <- (subset(model, X == "beta_s[1]")$mean + subset(model, X == "beta_s[5]")$mean + subset(model, X == "beta_s[3]")$mean+ subset(model, X == "beta_s[10]")$mean)
model.graph[model.graph$Parameter == "Sigma" & model.graph$Age == "3-yr-old" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta_s[1]")$mean + subset(model, X == "beta_s[2]")$mean+ subset(model, X == "beta_s[5]")$mean  + subset(model, X == "beta_s[3]")$mean + subset(model, X == "beta_s[6]")$mean+ subset(model, X == "beta_s[8]")$mean+ subset(model, X == "beta_s[10]")$mean   + subset(model, X == "beta_s[12]")$mean)

######## Tau
model.graph[model.graph$Parameter == "Tau" & model.graph$Age == "Adult" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Match",]$Value <- subset(model, X == "beta_t[1]")$mean
model.graph[model.graph$Parameter == "Tau" & model.graph$Age == "Adult" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta_t[1]")$mean + subset(model, X == "beta_t[2]")$mean)
model.graph[model.graph$Parameter == "Tau" & model.graph$Age == "Adult" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Match",]$Value <- (subset(model, X == "beta_t[1]")$mean + subset(model, X == "beta_t[3]")$mean)
model.graph[model.graph$Parameter == "Tau" & model.graph$Age == "Adult" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta_t[1]")$mean + subset(model, X == "beta_t[2]")$mean + subset(model, X == "beta_t[3]")$mean + subset(model, X == "beta_t[6]")$mean)

model.graph[model.graph$Parameter == "Tau" & model.graph$Age == "5-yr-old" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Match",]$Value <- (subset(model, X == "beta_t[1]")$mean + subset(model, X == "beta_t[4]")$mean)
model.graph[model.graph$Parameter == "Tau" & model.graph$Age == "5-yr-old" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta_t[1]")$mean + subset(model, X == "beta_t[4]")$mean + subset(model, X == "beta_t[2]")$mean  + subset(model, X == "beta_t[7]")$mean)
model.graph[model.graph$Parameter == "Tau" & model.graph$Age == "5-yr-old" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Match",]$Value <- (subset(model, X == "beta_t[1]")$mean + subset(model, X == "beta_t[4]")$mean + subset(model, X == "beta_t[3]")$mean+ subset(model, X == "beta_t[9]")$mean)
model.graph[model.graph$Parameter == "Tau" & model.graph$Age == "5-yr-old" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta_t[1]")$mean + subset(model, X == "beta_t[2]")$mean+ subset(model, X == "beta_t[4]")$mean  + subset(model, X == "beta_t[3]")$mean + subset(model, X == "beta_t[6]")$mean+ subset(model, X == "beta_t[7]")$mean+ subset(model, X == "beta_t[9]")$mean   + subset(model, X == "beta_t[11]")$mean)

model.graph[model.graph$Parameter == "Tau" & model.graph$Age == "3-yr-old" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Match",]$Value <- (subset(model, X == "beta_t[1]")$mean + subset(model, X == "beta_t[5]")$mean)
model.graph[model.graph$Parameter == "Tau" & model.graph$Age == "3-yr-old" & model.graph$AnswerType == "Predictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta_t[1]")$mean + subset(model, X == "beta_t[5]")$mean + subset(model, X == "beta_t[2]")$mean  + subset(model, X == "beta_t[8]")$mean)
model.graph[model.graph$Parameter == "Tau" & model.graph$Age == "3-yr-old" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Match",]$Value <- (subset(model, X == "beta_t[1]")$mean + subset(model, X == "beta_t[5]")$mean + subset(model, X == "beta_t[3]")$mean+ subset(model, X == "beta_t[10]")$mean)
model.graph[model.graph$Parameter == "Tau" & model.graph$Age == "3-yr-old" & model.graph$AnswerType == "Unpredictable" & 
              model.graph$SceneType == "Mismatch",]$Value <- (subset(model, X == "beta_t[1]")$mean + subset(model, X == "beta_t[2]")$mean+ subset(model, X == "beta_t[5]")$mean  + subset(model, X == "beta_t[3]")$mean + subset(model, X == "beta_t[6]")$mean+ subset(model, X == "beta_t[8]")$mean+ subset(model, X == "beta_t[10]")$mean   + subset(model, X == "beta_t[12]")$mean)

# Subtract constant to get real Values
model.graph[model.graph$Parameter == "Mu",]$Value <- model.graph[model.graph$Parameter == "Mu",]$Value - 725
model.graph$Age <- ordered(model.graph$Age, levels = c("Adult","5-yr-old","3-yr-old"))
ggplot(data = model.graph, aes(x = AnswerType, y = Value)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = SceneType)) +
  facet_wrap(~Parameter+Age)