# -*- coding: utf-8 -*-
"""UCLN.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1biCHPewROX7LaU6TFjUNlakbykxTN1sI
"""

a = int(input('nhap a = '))
b = int(input('nhap b = '))

def uscln(a, b):
    if (b == 0):
        return a;
    return uscln(b, a % b);
print('UCLN của',a,'và',b,'là:',uscln(a,b))