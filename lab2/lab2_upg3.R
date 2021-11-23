#setup
set.seed(12345)

df = read.csv("lab2/communities.csv")
head(df)
print(dim(df))
summary(df)

df_scaled = df.copy()

target = df_scaled$ViolentCrimesPerPop

scale(df_scaled)
summary(df_scaled)
df_scaled$ViolentCrimesPerPop = target 


summary(df_scaled)

summary(df)
