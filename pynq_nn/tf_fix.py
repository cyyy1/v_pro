# -*- coding: utf-8 -*-
#import input_data
from tensorflow.examples.tutorials.mnist import input_data
import tensorflow as tf
import numpy as np
import math
import struct

K=8

BIT_WIDTH=16

def Get_WeightLength(Ky,Kx,CHin,CHout):
	return (K*Kx*Ky*CHout*((CHin+K-1)/K))

def To_Fixed(tensor,bitwidth):
	array=tensor.eval()
	range=max(np.max(array),-np.min(array))
	int_part=max(math.ceil(math.log(range,2)+0.000001),0) + 1 #1 bit for sign
	fraction_part=bitwidth-int_part
	return ( np.round(array*pow(2,fraction_part)) , fraction_part ) #/pow(2,fraction_part) 

def Feature_To_Fixed(tensor,bitwidth,feed_dict):
	array=tensor.eval(feed_dict=feed_dict)
	range=max(np.max(array),-np.min(array))
	print(range)
	int_part=max(math.ceil(math.log(range,2)+0.000001),0) + 1 #1 bit for sign
	fraction_part=bitwidth-int_part
	return ( np.round(array*pow(2,fraction_part)) , fraction_part ) #/pow(2,fraction_part) 

def Map_Weight_Data(kernel,array_map,Ky,Kx,in_ch,out_ch):
	for cout in range(out_ch):
		for i in range(Ky):
			for j in range(Kx):
				for cin in range(in_ch):
					array_map[cout][i][j][cin//K][cin%K]=kernel[i][j][cin][cout]

def Get_Feature_Fraction_Part(tensor,name,feed_dict,file):
	(array,fraction_part)=Feature_To_Fixed(tensor,BIT_WIDTH,feed_dict)
	file.write("%s=%d\n" % ("PTR_"+name.upper(),int(fraction_part)) )
	print(name+' fraction_part: ' + str(int(fraction_part)))

def Record_Weight(tensor,name,file):
	(array,fraction_part)=To_Fixed(tensor,BIT_WIDTH)
	file.write("%s=%d\n" % ("PTR_"+name.upper(),int(fraction_part)) )
	array_map=np.zeros([np.shape(array)[3],np.shape(array)[0],np.shape(array)[1],(np.shape(array)[2]+K-1)//K,K])
	Map_Weight_Data(array,array_map,np.shape(array)[0],np.shape(array)[1],np.shape(array)[2],np.shape(array)[3])
	with open('./record/'+name+'.bin', 'wb') as fp:
		for i in range(np.shape(array_map)[0]):
			for j in range(np.shape(array_map)[1]):
				for k in range(np.shape(array_map)[2]):
					for l in range(np.shape(array_map)[3]):
						for m in range(np.shape(array_map)[4]):
							a=struct.pack('h',int(array_map[i][j][k][l][m]))
							#print(array_map[i][j][k][l][m]);
							fp.write(a)

