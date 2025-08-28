#安装包
install.packages("tidyr", "ggplot2", "classInt", "openxlsx")

#加载包
library(openxlsx)
library(ggplot2)
library(tidyr)
library(classInt)

#生成随机数据，用户可读取自己的数据
intensity <- data.frame(
  category = paste0("category", 1:6),
  setNames(data.frame(matrix(sample(0:20, 6*7, T), nrow=6)), paste0("interval", 1:7))
)

deviation <- data.frame(
  category = paste0("category", 1:6),
  setNames(data.frame(matrix(sample(-10:10, 6*7, T), nrow=6)), paste0("interval", 1:7))
)

#导出随机生成的数据
write.xlsx(list(intensity=intensity, deviation=deviation), "data.xlsx", overwrite=TRUE)

#请注意，上面是随机生成的数据，请使用下面的代码导入自己的数据
#使用read.xlsx函数读取自己的数据
intensity <- read.xlsx("data.xlsx",sheet = "intensity")
deviation <- read.xlsx("data.xlsx",sheet = "deviation")

#将强度数据转换为长格式
data <- pivot_longer(
  intensity, cols = 2:8, #读取第2-8列
  names_to = "interval", # 设置列名
  values_to = "intensity") #设置数值列名

#将强度偏差数据转换为长格式
data$deviation <- pivot_longer(
  deviation, cols = 2:8, #解析同上
  values_to = "deviation")$deviation #最后的参数是将结果导入data中

#intensity数据自然断点,设置分为5类，修改n的数值可更改类别数量
intensity_classes <- classIntervals(data$intensity, n = 5, style = "jenks")

#根据断点值重分类intensity数据
data$intensity_jenks <- findInterval(
  data$intensity,intensity_classes$brks, rightmost.closed=TRUE)

#将大小改为离散数值，方便后面自定义点的大小
data$intensity_jenks <- factor(data$intensity_jenks)

#将结果写入数据框
classes <- data.frame(
  intensity_jenks = intensity_classes$brks,
  stringsAsFactors = FALSE
)

#将断点结果导出
write.table(classes,file = "classes.csv",sep = ",",row.names = F) 
#请注意，自然断点法分类并不一定适合你的数据
#如有需要，可以自行调整分类方法，具体可以在Excel文件中手动分类

#定义y轴绘制顺序
data$category <- factor(data$category,
                        levels = c("category6","category5","category4",
                                   "category3","category2","category1"))

#设置颜色绘图环境
color_scale <- 
  scale_color_gradientn(colors = c("#212c5f","#d1eaf8","#ffebee", "#ff6a46"),
                        values = scales::rescale(c(min(data$deviation), -0.01,
                                                   0.01,max(data$deviation))),
                        limits = c(min(data$deviation), max(data$deviation)))

#绘制Intensity Deviation Maps
p1 <- ggplot(data = data)+
  
  #添加水平背景线
  geom_hline(yintercept = c(1.5,2.5,3.5,4.5,5.5),
             color = "grey60",
             linewidth = 0.5,
             linetype = "dashed") + 
  
  #添加垂直分隔线
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5,6.5),
             color="grey60",
             linewidth=0.5,
             linetype = "dashed") + 
  
  #画大小不同的点
  geom_point(mapping = aes(x = interval,
                           y = category, 
                           size = intensity_jenks,
                           color = deviation)) + 
  
  #给点加上黑色轮廓
  geom_point(mapping = aes(x = interval,
                           y = category, 
                           size = intensity_jenks,
                           color = deviation),
             colour = "black",
             shape = 21) + 
  #选择前面设置好的颜色映射
  color_scale+
  
  #设置点的大小
  scale_size_manual(values = c("1"=5,"2"=7,"3"=9,"4"=11,"5"=13)) +
  
  #设置x轴
  scale_x_discrete(position = "top",expand = c(0.075,0.075))  +  
  
  scale_y_discrete(expand = c(0.075,0.075)) +
  
  # 取消X轴和Y轴标题,设置图例的名称
  labs(
    x = NULL, 
    y = NULL,
    color = "Intensity deviation",  # 设置颜色图例的名称
    size = "Intensity"    # 设置大小图例的名称
  ) +  
  
  #设置主题
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA,
                                    color = "black",
                                    linewidth = 1 ), #设置边框
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 0, hjust = 0),
        axis.text = element_text(size = 12,
                                 color = "black"),
        #设置坐标轴标题字体的颜色、大小和字体
        axis.ticks.x = element_line(color = "grey40",
                                    linewidth = 0.5), 
        axis.ticks.y = element_line(color = "grey40",
                                    linewidth = 0.5), #设置刻度线的颜色和大小
        axis.ticks.length.x = unit(4,"pt"), #设置x轴刻度线的长度
        axis.ticks.length.y = unit(5,"pt"), #设置y轴刻度线的长度
        plot.margin = margin(20,30,20,30), #设置图的边距
        legend.position = 'right'
  )

#导出Intensity Deviation Maps
#width和height分别设置宽和高，添加family="serif"可以设置Times New Roman字体
pdf("Intensity Deviation Maps.pdf",width = 7.8,height = 5)
print(p1)
dev.off()
