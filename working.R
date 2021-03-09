packages<-c("readxl","tidyverse","plotly","rlist","rio","janitor","data.table","openxlsx")
sapply(packages,library,character.only=T)

#Read_data
df1<-read.xlsx("HBS_Table_No._206___Forward_Premia_(Inter-Bank)_(Monthly_Average).xlsx")
names(df1)<-df1[2,]
df1<-df1[-c(1:2,305:nrow(df1)),]
dates<-rev(seq(as.Date("1995-06-01"),as.Date("2020-07-01"),"month"))
df2<-cbind(df1,dates)
df2<-df2 %>% clean_names() %>% select(-c(year,month)) %>%
        select(dates,everything())
num_fn<-function(x){
        x<-as.numeric(x)
        x<-round(x,3)
}
df2[,-1]<-lapply(df2[,-1],num_fn)
p1<-df2 %>% plot_ly()
p2<-p1 %>% add_trace(x=~dates,y=~x3_month,type="scatter",mode="lines",
                     name="3- Month Premium",
                     hoverinfo = "text",
                     text = ~paste(x3_month, ' %')
                     ) %>% 
        add_trace(x=~dates,y=~x6_month,type="scatter",mode="lines",
                  name="6- Month Premium",
                  hoverinfo = "text",
                  text = ~paste(x6_month, ' %')
        ) %>% 
        add_trace(x=~dates,y=~x1_month,type="scatter",mode="lines",
                  name="1- Month Premium",
                  hoverinfo = "text",
                  text = ~paste(x1_month, ' %')
        ) %>% 
        layout(title="Forward exchange rate premia over spot rates",
               xaxis=list(type="date",
                          tickformat="%b-%Y",title="Date"),
               yaxis=list(title="Premium (in %)"))
typeof(df2$x3_month)

df3<-df2 %>% pivot_longer(-dates,names_to = "Series",values_to = "Premium")

