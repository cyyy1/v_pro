


# 基于UVM的MCDF验证环境搭建
## 硬件设计描述
### 功能描述
DUT为多通道数据整形器(MCDF，multi-channel data formatter)，它可以将上行(uplink)多个通道数据经过内部的FIFO，最终以数据包(data packet)的形式送出。此外，多通道数据整形器也有寄存器的读写接口，可以支持更多的控制功能。本项目将原有的寄存器接口修改为APB接口，对硬件功能进行配置。

MCDF的结构来看主要可以分为如下几个部分：

 - 上行数据的通道从端(Channel Slave)，负责接收上行数据，并且存储到与之对应的FIFO中。
 - 仲裁器(Arbiter)可以选择从不同的FIFO中读取数据，进而将数据进一步传送至整形器(formatter)。
 - 整形器(Formatter)将数据按照一定的接口时序送出至下行接收端。
 - 控制寄存器(Control Registers)有专用的寄存器读写接口，负责接收命令并且对MCDF的功能做出修改。
### 寄存器描述
地址0x00 通道1控制寄存器 32bits 读写寄存器
 - bit(0)：通道使能信号。1为打开，0位关闭。复位值为1。
 - bit(2:1)：优先级。0为最高，3为最低。复位值为3。
 - bit(5:3)：数据包长度，解码对应表为， 0对应长度4， 1对应长度8，2对应长度16，3对应长度32，其它数值（4-7）均暂时对应长度32。复位值为0。
 - bit(31:6)：保留位，无法写入。复位值为0。

地址0x04 通道2控制寄存器 32bits 读写寄存器和地址0x08 通道3控制寄存器 32bits 读写寄存器同通道1控制寄存器描述。

 地址0x10 通道1状态寄存器 32bits 只读寄存器
 - bit(7:0)：上行数据从端FIFO的可写余量，同FIFO的数据余量保持同步变化。复位值为FIFO的深度数。
 - bit(31:8)：保留位，复位值为0。

地址0x14 通道2状态寄存器 32bits 只读寄存器和地址0x18 通道3状态寄存器 32bits 只读寄存器同通道1状态寄存器描述。

## 测试平台构建
![在这里插入图片描述](https://img-blog.csdnimg.cn/20200702230510409.jpg?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L3FxXzQxOTcwNTEy,size_16,color_FFFFFF,t_70#pic_center)
![在这里插入图片描述](https://img-blog.csdnimg.cn/20200702224446816.jpg?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L3FxXzQxOTcwNTEy,size_16,color_FFFFFF,t_70#pic_center)
## 验证计划表格
| 测试功能点 |  测试内容 | 测试类名
|--|--|--|
| 寄存器复位测试 | 随机化寄存器模型并进行更新，然后将rstn拉高，读取所有寄存器的值，检查是否与设计规定的复位值相同 | mcdf_reg_reset_test
| 寄存器读写测试 | 所有控制寄存器的读写测试和所有状态寄存器的读写测试 | mcdf_reg_read_write_test
| 寄存器稳定性测试 | 对控制寄存器的保留域进行读写，确定寄存器的值是预期值而不是紊乱值 | mcdf_reg_stability_test
| 寄存器非法访问测试 | 非法地址读写，对状态寄存器进行写操作，非法寄存器操作不能影响MCDF的整体功能 | mcdf_reg_illegal_access_test
| 数据通道开关测试 | 对每一个数据通道对应的控制寄存器域en配置为0，在关闭状态下测试数据写入是否通过 | mcdf_channel_disable_test
| 优先级测试 | 将不同数据通道配置为相同或不同的优先级，在数据通道使能的情况下进行测试 | mcdf_arbiter_priority_test
| 发包长度测试 | 将不同数据通道随机配置为各自的长度，在通道使能的情况下测试，从formatter发送出来的打包长度应该同对应通道寄存器的配置值保持一致 | mcdf_formatter_length_test
| 下行从端低带宽测试 | 将MCDF下行数据接收端设置为小存储和低带宽的类型，使得formatter 发送数据后，下行从端有更多的机会延迟grant信号，用来模拟真实场景| mcdf_down_stream_low_bandwidth_test
| 全随机测试 | 将不同数据通道的使能，打包长度，优先级全部设置为随机进行测试| mcdf_full_random_test
## 覆盖率收集
![在这里插入图片描述](https://img-blog.csdnimg.cn/20200702222021797.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L3FxXzQxOTcwNTEy,size_16,color_FFFFFF,t_70#pic_center)
![在这里插入图片描述](https://img-blog.csdnimg.cn/20200702222130147.png#pic_center)
# 基于PYNQ-Z2的卷积神经网络设计
## 硬件架构设计
## 软件设计
## 系统性能评估
