library(tidyverse)
library(repurrrsive)
library(dplyr)
library(httr)
library(gh)
library(jsonlite)
library(foreach)
library(tibble)
library(magrittr)
library(purrr)
library(lubridate)
library(DT)
library(ggplot2)

git_mytoken = "6b56ad20155d011bca31cbf5af33090b6d97834e"
Sys.setenv(GITHUB_TOKEN = git_mytoken)
json_git_user = gh("/users/beck")
options(encoding = 'UTF-8')

git_user_path <- "./repo/"
user_repos_list = gh("/users/beck/repos", .limit=Inf) 
for(r in 1:length(user_repos_list))
{
  export_git_user_json = toJSON(user_repos_list[r],pretty = TRUE)
  write(export_git_user_json,paste(git_user_path,"user_repos_",r,".json",sep=""))
}

git_user_path <- "./followers/"
user_followers_list = gh("/users/beck/followers", .limit=Inf)
# Collecting all the followers in JSON files on local desktop
for(i in 1:length(user_followers_list)) 
{
  export_json = toJSON(user_followers_list[i],pretty = TRUE)
  write(export_json,paste(git_user_path,"user_followers_",i,".json",sep=""))
}

issue_path <- "./issue/"
for(i in 1:length(user_repos_list)){
  if (user_repos_list[[i]]$open_issues_count > 0){
    write(toJSON(gh("/repos/:owner/:repo/issues",
                    owner = 'beck',
                    issue= 'all',
                    state= 'all',
                    repo = user_repos_list[[i]]$name,
                    .token = "6b56ad20155d011bca31cbf5af33090b6d97834e"), 
                 pretty = TRUE),
          file.path(issue_path, 
                    paste("user_repos_list",i,".json",sep="")))
  }}

users_information = gh("/users/beck")
user_df <- users_information %>%
  
  toJSON() %>%
  fromJSON() %>%
  
  purrr::flatten() %>%
  
  map_if(is_list, as_tibble) %>%
  
  map_if(is_tibble, list) %>%
  
  bind_cols() %>%
  as.data.frame

user_df = user_df %>% select(id, name, public_repos, followers)
datatable(user_df)

followers_list_beck <- gh("/users/beck/followers", .limit=Inf)

follower = map_df(followers_list_beck, magrittr::extract, names(followers_list_beck[[1]]))

followers_details = 
  map(follower$login, ~gh(paste0("/users/", .)))

Beck_followers_details <- map_df(followers_details,magrittr::extract,c('login','id','public_repos','followers'))
datatable(Beck_followers_details)

path <- "./repo/"
files <- dir(path, pattern = "*.json")
repo_df <- files %>%
  map_df(~fromJSON(file.path(path, .), flatten = TRUE))
repo_new=repo_df%>% select(name,language,size, forks_count, stargazers_count,
                           watchers_count,open_issues_count)
datatable(repo_new)

issues_path <- "./issue/"
git_Issues <- dir(issues_path, pattern = "*.json")
git_Issues = git_Issues %>% file.path(issues_path,.)

issues = list()
for(i in 1:length(git_Issues)) {
  df = fromJSON(git_Issues[[i]], flatten = TRUE)
  if (!df[1,1]==""){
    issues[[i]] = df
  }
}

Filtered_issues = bind_rows(issues)
df_issues = as_tibble(Filtered_issues)

colnames(df_issues)
df_issues <- df_issues %>% mutate(created_at = ymd_hms(created_at))
df_issues <- df_issues %>% mutate(updated_at = ymd_hms(updated_at))

Merged_df = merge(x = repo_df, y = df_issues, by.x="url",by.y="repository_url",sort=F)

updated_issue = Merged_df %>% select(name,state,open_issues_count,open_issues,created_at.y,
                                     updated_at.y,pushed_at) %>% 
  mutate(issue_created_at = ymd_hms(created_at.y), issue_updated_at = ymd_hms(updated_at.y))

updated_issue = updated_issue %>% mutate(age=issue_updated_at-issue_created_at) %>% 
  mutate(avg_duration=mean(age)) %>% 
  mutate(year=substr(updated_at.y,0,4)) %>% 
  mutate(month=substr(updated_at.y,6,7))

updated_issue = updated_issue %>% 
  select(name,state,open_issues_count,avg_duration,month,year,age,pushed_at) 

datatable(updated_issue)


theme_set(theme_classic())

ggplot(repo_new, aes(x=as.character(language), y=size)) + 
  geom_bar(stat="identity", width=.9, fill="tomato3") + 
  labs(title="Bar Chart", 
       subtitle="Language used vs Size", 
       caption="source: https://github.com/beck") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.2))

ggplot(df_issues, aes(x = as.character(state),y=number))+
  geom_bar(stat="identity")+
  labs(title="Bar Chart", 
       subtitle="Open and CLosed Trends", 
       caption="source: https://github.com/beck") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.2))

ggplot(updated_issue, aes(x = as.numeric(year),y="", fill = as.character(state)))+
  geom_bar(stat="identity")+
  labs(title="Bar Chart", 
       subtitle="Open and CLosed Trends", 
       caption="source: https://github.com/beck") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.2))
