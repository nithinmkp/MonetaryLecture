packages<-c("readxl","tidyverse","plotly","rlist","rio","janitor","data.table","openxlsx","here")
sapply(packages,library,character.only=T)

#new data
files<-list.files(path=here(),pattern = "*.csv")
data<-map(files,read.csv)
names<-c("Spot Rate","Domestic Rate","Foreign Rate")
data<-map2(data,names,~setnames(.x,2,.y))
data<-map(data,~rename(.x,Date=DATE))
data2<-list.cbind(data) %>% select(1,2,4,6)

#forward rates
data3<-data2 %>% mutate(forward_rate=lead(`Spot Rate`,n=12))
data3<-data3 %>% filter(Date<=as.Date("2019-12-01"))
data3<-data3 %>% 
        mutate(int_diff=((1+data3$`Domestic Rate`/100)-(1+data3$`Foreign Rate`/100))*data3$forward_rate/data3$`Spot Rate`)

data3$Date<-as.Date(data3$Date)
num_fn<-function(x){
        x<-as.numeric(x)
        x<-round(x,3)
}
data3[,-1]<-lapply(data3[,-1],num_fn)
p<-data3 %>% ggplot(aes(x=Date,y=int_diff))+
        geom_line()
q<-data3 %>% plot_ly()
r<-q %>% add_trace(x=~Date,y=~int_diff,type="scatter",mode="lines",
                   name="31-Year Differential",
                   hoverinfo = "text",
                   text = ~paste(int_diff, ' %')
) %>% 
        layout(title="Interest rate differential",
               xaxis=list(type="date",
                          tickformat="%b-%Y",title="Date"),
               yaxis=list(title="differnital (in %)"))
