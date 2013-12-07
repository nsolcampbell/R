#作为一个R迷，为什么要去捣鼓XLS文件？其实这种需求场景很多的啦，比如其它部门的同事有批量的Excel文件要处理，或者家里一把手的直接命令。
#Excel里面已经有不少函数可以处理数据了，包括简单的矩阵运算以及透视表什么的，但归根到底它还是需要鼠标点来点去，伤手腕啊。
#为了保护右手我们要提倡用代码控制一切需要鼠标的动作。高级的Excel玩家可能会用VBA去做自动处理，更高明的玩家则跳出三界外，
#从外部来控制单位格数据的输入输出。

#R语言中有很多包可以处理表格文档，包括最为通用的RODBC包，XLConnect包也是操控Excel文档的利器，功能很丰富。
#使用该包的前提是要安装好Java，还要在环境变量里搞好设置。之后就可以安装加载包了。
#我们可以将表格文档看做是数据输入和输出端，R则是中间的运算单元。
#二者主要是通过数据框格式和工作表单元格进行交换。下面来看将iris数据框写入和读取的示例（其实是翻译的官方文档）。


#从层级上来看，Excel文档数据有三个级别，分别是文档(file)、工作表(sheet)、区域(region)。所以从R写入数据也有几个步骤。


install.packages("RODBC")
library("RODBC")
install.packages("XLConnect")
library("XLConnect")


# 读取或创建一个XLSX文件，此步相当于建立一个连接
xls <- loadWorkbook('test.xlsx',create=TRUE) 
# 创建工作表
createSheet(xls,name='namesheet')
# 写入数据
writeWorksheet(xls,iris,'nameshee',
               startRow=1,startCol=1, # 数据出现的左上角位置
               header=TRUE)
# 存入硬盘，直到此步方才有文档生成
saveWorkbook(xls)

#上面四个步骤是新建文档、新建工作表、写入数据、最后存盘。如果要写入数据的同时创建好区域名称，则在第三步有所不同。
# 创建区域名
createName(xls,name='nameregion',
           formula='namesheet!$C$5', #区域的左上角单元格位置
           overwrite=TRUE)
# 写入数据
writeNamedRegion(xls,iris,name='nameregion')

#读取文档则简单的多

xls <- loadWorkbook('test.xlsx',create=TRUE)
data <- readWorksheet(xls, 'namesheet',
              startRow=1, startCol=1,
              endRow=0,endCol=0, #取0表示自动判断
              header=TRUE)
#上面读写文档都有多个步骤，如果想一步到位，也有相应的快捷函数。比较好玩的是还可以往Excel中插入图片。
#我们可以先利用R的绘图能力，建立好一张图片，例如叫作iris.png，再向表格中的区域位置插入图片。
xls <- loadWorkbook('test.xlsx',create=TRUE)
addImage(xls,'iris.png',name='nameregion',originalSize=TRUE)
saveWorkbook(xls)
#使用demo(addImage)可以查看详细的演示过程。
