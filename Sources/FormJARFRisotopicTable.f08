subroutine FormJARFRisotopicTable
    use CommonModJARFR
    allocate(NuclDataCommon(1:99))
    !JUST NUMBER OF ORDER
    do i=1,99
      NuclDataCommon(i)%nucl_order_num = i
    end do
    !INNER ID (FROM AN INPUT FILE)
    NuclDataCommon(1)%nucl_numberID = 1
    NuclDataCommon(2)%nucl_numberID = 2
    NuclDataCommon(3)%nucl_numberID = 18
    NuclDataCommon(4)%nucl_numberID = 5
    NuclDataCommon(5)%nucl_numberID = 4
    NuclDataCommon(6)%nucl_numberID = 6
    NuclDataCommon(7)%nucl_numberID = 7
    NuclDataCommon(8)%nucl_numberID = 9
    NuclDataCommon(9)%nucl_numberID = 10
    NuclDataCommon(10)%nucl_numberID = 11
    NuclDataCommon(11)%nucl_numberID = 12
    NuclDataCommon(12)%nucl_numberID = 14
    NuclDataCommon(13)%nucl_numberID = 15
    NuclDataCommon(14)%nucl_numberID = 16
    NuclDataCommon(15)%nucl_numberID = 19
    NuclDataCommon(16)%nucl_numberID = 23
    NuclDataCommon(17)%nucl_numberID = 24
    NuclDataCommon(18)%nucl_numberID = 27
    NuclDataCommon(19)%nucl_numberID = 28
    NuclDataCommon(20)%nucl_numberID = 31
    NuclDataCommon(21)%nucl_numberID = 32
    NuclDataCommon(22)%nucl_numberID = 35
    NuclDataCommon(23)%nucl_numberID = 39
    NuclDataCommon(24)%nucl_numberID = 40
    NuclDataCommon(25)%nucl_numberID = 48
    NuclDataCommon(26)%nucl_numberID = 51
    NuclDataCommon(27)%nucl_numberID = 52
    NuclDataCommon(28)%nucl_numberID = 55
    NuclDataCommon(29)%nucl_numberID = 56
    NuclDataCommon(30)%nucl_numberID = 57
    NuclDataCommon(31)%nucl_numberID = 59
    NuclDataCommon(32)%nucl_numberID = 64
    NuclDataCommon(33)%nucl_numberID = 70
    NuclDataCommon(34)%nucl_numberID = 89
    NuclDataCommon(35)%nucl_numberID = 91
    NuclDataCommon(36)%nucl_numberID = 93
    NuclDataCommon(37)%nucl_numberID = 96
    NuclDataCommon(38)%nucl_numberID = 99
    NuclDataCommon(39)%nucl_numberID = 103
    NuclDataCommon(40)%nucl_numberID = 108
    NuclDataCommon(41)%nucl_numberID = 109
    NuclDataCommon(42)%nucl_numberID = 112
    NuclDataCommon(43)%nucl_numberID = 113
    NuclDataCommon(44)%nucl_numberID = 122
    NuclDataCommon(45)%nucl_numberID = 131
    NuclDataCommon(46)%nucl_numberID = 135
    NuclDataCommon(47)%nucl_numberID = 133
    NuclDataCommon(48)%nucl_numberID = 143
    NuclDataCommon(49)%nucl_numberID = 145
    NuclDataCommon(50)%nucl_numberID = 147
    NuclDataCommon(51)%nucl_numberID = 149
    NuclDataCommon(52)%nucl_numberID = 151
    NuclDataCommon(53)%nucl_numberID = 150
    NuclDataCommon(54)%nucl_numberID = 152
    NuclDataCommon(55)%nucl_numberID = 155
    NuclDataCommon(56)%nucl_numberID = 157
    NuclDataCommon(57)%nucl_numberID = 156
    NuclDataCommon(58)%nucl_numberID = 158
    NuclDataCommon(59)%nucl_numberID = 162
    NuclDataCommon(60)%nucl_numberID = 163
    NuclDataCommon(61)%nucl_numberID = 179
    NuclDataCommon(62)%nucl_numberID = 171
    NuclDataCommon(63)%nucl_numberID = 181
    NuclDataCommon(64)%nucl_numberID = 176
    NuclDataCommon(65)%nucl_numberID = 197
    NuclDataCommon(66)%nucl_numberID = 207
    NuclDataCommon(67)%nucl_numberID = 209
    NuclDataCommon(68)%nucl_numberID = 232
    NuclDataCommon(69)%nucl_numberID = 211
    NuclDataCommon(70)%nucl_numberID = 213
    NuclDataCommon(71)%nucl_numberID = 233
    NuclDataCommon(72)%nucl_numberID = 234
    NuclDataCommon(73)%nucl_numberID = 235
    NuclDataCommon(74)%nucl_numberID = 236
    NuclDataCommon(75)%nucl_numberID = 238
    NuclDataCommon(76)%nucl_numberID = 227
    NuclDataCommon(77)%nucl_numberID = 229
    NuclDataCommon(78)%nucl_numberID = 246
    NuclDataCommon(79)%nucl_numberID = 248
    NuclDataCommon(80)%nucl_numberID = 239
    NuclDataCommon(81)%nucl_numberID = 240
    NuclDataCommon(82)%nucl_numberID = 241
    NuclDataCommon(83)%nucl_numberID = 242
    NuclDataCommon(84)%nucl_numberID = 201
    NuclDataCommon(85)%nucl_numberID = 202
    NuclDataCommon(86)%nucl_numberID = 203
    NuclDataCommon(87)%nucl_numberID = 222
    NuclDataCommon(88)%nucl_numberID = 223
    NuclDataCommon(89)%nucl_numberID = 224
    NuclDataCommon(90)%nucl_numberID = 225
    NuclDataCommon(91)%nucl_numberID = 226
    NuclDataCommon(92)%nucl_numberID = 228
    NuclDataCommon(93)%nucl_numberID = 214
    NuclDataCommon(94)%nucl_numberID = 215
    NuclDataCommon(95)%nucl_numberID = 218
    NuclDataCommon(96)%nucl_numberID = 301
    NuclDataCommon(97)%nucl_numberID = 302
    NuclDataCommon(98)%nucl_numberID = 303
    NuclDataCommon(99)%nucl_numberID = 304


    !CAPTION
    NuclDataCommon(1)%nucl_Caption = 'H'
    NuclDataCommon(2)%nucl_Caption = 'D'
    NuclDataCommon(3)%nucl_Caption = 'AR'
    NuclDataCommon(4)%nucl_Caption = 'HE-3'
    NuclDataCommon(5)%nucl_Caption = 'HE'
    NuclDataCommon(6)%nucl_Caption = 'LI-6'
    NuclDataCommon(7)%nucl_Caption = 'LI-7'
    NuclDataCommon(8)%nucl_Caption = 'BE'
    NuclDataCommon(9)%nucl_Caption = 'B-10'
    NuclDataCommon(10)%nucl_Caption = 'B-11'
    NuclDataCommon(11)%nucl_Caption = 'C'
    NuclDataCommon(12)%nucl_Caption = 'N'
    NuclDataCommon(13)%nucl_Caption = 'N-15'
    NuclDataCommon(14)%nucl_Caption = 'O'
    NuclDataCommon(15)%nucl_Caption = 'F'
    NuclDataCommon(16)%nucl_Caption = 'NA'
    NuclDataCommon(17)%nucl_Caption = 'MG'
    NuclDataCommon(18)%nucl_Caption = 'AL'
    NuclDataCommon(19)%nucl_Caption = 'SI'
    NuclDataCommon(20)%nucl_Caption = 'P'
    NuclDataCommon(21)%nucl_Caption = 'S'
    NuclDataCommon(22)%nucl_Caption = 'CL'
    NuclDataCommon(23)%nucl_Caption = 'K'
    NuclDataCommon(24)%nucl_Caption = 'CA'
    NuclDataCommon(25)%nucl_Caption = 'TI'
    NuclDataCommon(26)%nucl_Caption = 'V'
    NuclDataCommon(27)%nucl_Caption = 'CR'
    NuclDataCommon(28)%nucl_Caption = 'MN'
    NuclDataCommon(29)%nucl_Caption = 'FE'
    NuclDataCommon(30)%nucl_Caption = 'CO'
    NuclDataCommon(31)%nucl_Caption = 'NI'
    NuclDataCommon(32)%nucl_Caption = 'CU'
    NuclDataCommon(33)%nucl_Caption = 'GA'
    NuclDataCommon(34)%nucl_Caption = 'Y'
    NuclDataCommon(35)%nucl_Caption = 'ZR'
    NuclDataCommon(36)%nucl_Caption = 'NB'
    NuclDataCommon(37)%nucl_Caption = 'MO'
    NuclDataCommon(38)%nucl_Caption = 'TC99'
    NuclDataCommon(39)%nucl_Caption = 'RH'
    NuclDataCommon(40)%nucl_Caption = 'AG'
    NuclDataCommon(41)%nucl_Caption = 'AG09'
    NuclDataCommon(42)%nucl_Caption = 'CD'
    NuclDataCommon(43)%nucl_Caption = 'CD13'
    NuclDataCommon(44)%nucl_Caption = 'SB'
    NuclDataCommon(45)%nucl_Caption = 'XE31'
    NuclDataCommon(46)%nucl_Caption = 'XE35'
    NuclDataCommon(47)%nucl_Caption = 'CS'
    NuclDataCommon(48)%nucl_Caption = 'ND43'
    NuclDataCommon(49)%nucl_Caption = 'ND45'
    NuclDataCommon(50)%nucl_Caption = 'PM47'
    NuclDataCommon(51)%nucl_Caption = 'SM49'
    NuclDataCommon(52)%nucl_Caption = 'SM51'
    NuclDataCommon(53)%nucl_Caption = 'SM52'
    NuclDataCommon(54)%nucl_Caption = 'EU'
    NuclDataCommon(55)%nucl_Caption = 'EU55'
    NuclDataCommon(56)%nucl_Caption = 'GD'
    NuclDataCommon(57)%nucl_Caption = 'GD55'
    NuclDataCommon(58)%nucl_Caption = 'GD57'
    NuclDataCommon(59)%nucl_Caption = 'DY'
    NuclDataCommon(60)%nucl_Caption = 'ER'
    NuclDataCommon(61)%nucl_Caption = 'HF'
    NuclDataCommon(62)%nucl_Caption = 'TA'
    NuclDataCommon(63)%nucl_Caption = 'W'
    NuclDataCommon(64)%nucl_Caption = 'RE'
    NuclDataCommon(65)%nucl_Caption = 'AU'
    NuclDataCommon(66)%nucl_Caption = 'PB'
    NuclDataCommon(67)%nucl_Caption = 'BI'
    NuclDataCommon(68)%nucl_Caption = 'TH32'
    NuclDataCommon(69)%nucl_Caption = 'PA31'
    NuclDataCommon(70)%nucl_Caption = 'PA33'
    NuclDataCommon(71)%nucl_Caption = 'U233'
    NuclDataCommon(72)%nucl_Caption = 'U234'
    NuclDataCommon(73)%nucl_Caption = 'U235'
    NuclDataCommon(74)%nucl_Caption = 'U236'
    NuclDataCommon(75)%nucl_Caption = 'U238'
    NuclDataCommon(76)%nucl_Caption = 'NP37'
    NuclDataCommon(77)%nucl_Caption = 'NP39'
    NuclDataCommon(78)%nucl_Caption = 'PU36'
    NuclDataCommon(79)%nucl_Caption = 'PU38'
    NuclDataCommon(80)%nucl_Caption = 'PU39'
    NuclDataCommon(81)%nucl_Caption = 'PU40'
    NuclDataCommon(82)%nucl_Caption = 'PU41'
    NuclDataCommon(83)%nucl_Caption = 'PU42'
    NuclDataCommon(84)%nucl_Caption = 'AM41'
    NuclDataCommon(85)%nucl_Caption = 'AM2M'
    NuclDataCommon(86)%nucl_Caption = 'AM43'
    NuclDataCommon(87)%nucl_Caption = 'CM42'
    NuclDataCommon(88)%nucl_Caption = 'CM43'
    NuclDataCommon(89)%nucl_Caption = 'CM44'
    NuclDataCommon(90)%nucl_Caption = 'CM45'
    NuclDataCommon(91)%nucl_Caption = 'CM46'
    NuclDataCommon(92)%nucl_Caption = 'CM48'
    NuclDataCommon(93)%nucl_Caption = 'FP35'
    NuclDataCommon(94)%nucl_Caption = 'FP39'
    NuclDataCommon(95)%nucl_Caption = 'SLAG'
    NuclDataCommon(96)%nucl_Caption = 'CAPT'
    NuclDataCommon(97)%nucl_Caption = 'SCAT'
    NuclDataCommon(98)%nucl_Caption = 'D-SC'
    NuclDataCommon(99)%nucl_Caption = 'HYDR'


    !ATOMIC MASS
    NuclDataCommon(1)%nucl_atomic_mass = 1.0079
    NuclDataCommon(2)%nucl_atomic_mass = 2.01410
    NuclDataCommon(3)%nucl_atomic_mass = 39.94800
    NuclDataCommon(4)%nucl_atomic_mass = 3.01603
    NuclDataCommon(5)%nucl_atomic_mass = 4.00260
    NuclDataCommon(6)%nucl_atomic_mass = 6.01510
    NuclDataCommon(7)%nucl_atomic_mass = 7.01600
    NuclDataCommon(8)%nucl_atomic_mass = 9.01220
    NuclDataCommon(9)%nucl_atomic_mass = 10.01290
    NuclDataCommon(10)%nucl_atomic_mass = 11.00930
    NuclDataCommon(11)%nucl_atomic_mass = 12.011
    NuclDataCommon(12)%nucl_atomic_mass = 14.00670
    NuclDataCommon(13)%nucl_atomic_mass = 15.00011
    NuclDataCommon(14)%nucl_atomic_mass = 15.99940
    NuclDataCommon(15)%nucl_atomic_mass = 18.99840
    NuclDataCommon(16)%nucl_atomic_mass = 22.98980
    NuclDataCommon(17)%nucl_atomic_mass = 24.30500
    NuclDataCommon(18)%nucl_atomic_mass = 26.98150
    NuclDataCommon(19)%nucl_atomic_mass = 28.08550
    NuclDataCommon(20)%nucl_atomic_mass = 30.97380
    NuclDataCommon(21)%nucl_atomic_mass = 32.07000
    NuclDataCommon(22)%nucl_atomic_mass = 35.45300
    NuclDataCommon(23)%nucl_atomic_mass = 39.09830
    NuclDataCommon(24)%nucl_atomic_mass = 40.07800
    NuclDataCommon(25)%nucl_atomic_mass = 47.88000
    NuclDataCommon(26)%nucl_atomic_mass = 50.94150
    NuclDataCommon(27)%nucl_atomic_mass = 51.99600
    NuclDataCommon(28)%nucl_atomic_mass = 54.93800
    NuclDataCommon(29)%nucl_atomic_mass = 55.84700
    NuclDataCommon(30)%nucl_atomic_mass = 58.93320
    NuclDataCommon(31)%nucl_atomic_mass = 58.69000
    NuclDataCommon(32)%nucl_atomic_mass = 63.54600
    NuclDataCommon(33)%nucl_atomic_mass = 69.72300
    NuclDataCommon(34)%nucl_atomic_mass = 88.90585
    NuclDataCommon(35)%nucl_atomic_mass = 91.22400
    NuclDataCommon(36)%nucl_atomic_mass = 92.90640
    NuclDataCommon(37)%nucl_atomic_mass = 95.94000
    NuclDataCommon(38)%nucl_atomic_mass = 98.90625
    NuclDataCommon(39)%nucl_atomic_mass = 102.90550
    NuclDataCommon(40)%nucl_atomic_mass = 107.86820
    NuclDataCommon(41)%nucl_atomic_mass = 108.90480
    NuclDataCommon(42)%nucl_atomic_mass = 112.41000
    NuclDataCommon(43)%nucl_atomic_mass = 112.90440
    NuclDataCommon(44)%nucl_atomic_mass = 121.76000
    NuclDataCommon(45)%nucl_atomic_mass = 130.90508
    NuclDataCommon(46)%nucl_atomic_mass = 134.90721
    NuclDataCommon(47)%nucl_atomic_mass = 132.90540
    NuclDataCommon(48)%nucl_atomic_mass = 142.90981
    NuclDataCommon(49)%nucl_atomic_mass = 144.91257
    NuclDataCommon(50)%nucl_atomic_mass = 146.91513
    NuclDataCommon(51)%nucl_atomic_mass = 148.91718
    NuclDataCommon(52)%nucl_atomic_mass = 150.91994
    NuclDataCommon(53)%nucl_atomic_mass = 151.91972
    NuclDataCommon(54)%nucl_atomic_mass = 151.96500
    NuclDataCommon(55)%nucl_atomic_mass = 154.92290
    NuclDataCommon(56)%nucl_atomic_mass = 157.25000
    NuclDataCommon(57)%nucl_atomic_mass = 154.92262
    NuclDataCommon(58)%nucl_atomic_mass = 156.92397
    NuclDataCommon(59)%nucl_atomic_mass = 162.50000
    NuclDataCommon(60)%nucl_atomic_mass = 167.25999
    NuclDataCommon(61)%nucl_atomic_mass = 178.49001
    NuclDataCommon(62)%nucl_atomic_mass = 180.94791
    NuclDataCommon(63)%nucl_atomic_mass = 183.85001
    NuclDataCommon(64)%nucl_atomic_mass = 186.20700
    NuclDataCommon(65)%nucl_atomic_mass = 196.96651
    NuclDataCommon(66)%nucl_atomic_mass = 207.20000
    NuclDataCommon(67)%nucl_atomic_mass = 208.98038
    NuclDataCommon(68)%nucl_atomic_mass = 232.03810
    NuclDataCommon(69)%nucl_atomic_mass = 231.03587
    NuclDataCommon(70)%nucl_atomic_mass = 233.04024
    NuclDataCommon(71)%nucl_atomic_mass = 233.03960
    NuclDataCommon(72)%nucl_atomic_mass = 234.04089
    NuclDataCommon(73)%nucl_atomic_mass = 235.04390
    NuclDataCommon(74)%nucl_atomic_mass = 236.04559
    NuclDataCommon(75)%nucl_atomic_mass = 238.05080
    NuclDataCommon(76)%nucl_atomic_mass = 237.04817
    NuclDataCommon(77)%nucl_atomic_mass = 239.05293
    NuclDataCommon(78)%nucl_atomic_mass = 236.04605
    NuclDataCommon(79)%nucl_atomic_mass = 238.04961
    NuclDataCommon(80)%nucl_atomic_mass = 239.05220
    NuclDataCommon(81)%nucl_atomic_mass = 240.05380
    NuclDataCommon(82)%nucl_atomic_mass = 241.05679
    NuclDataCommon(83)%nucl_atomic_mass = 242.05870
    NuclDataCommon(84)%nucl_atomic_mass = 241.05679
    NuclDataCommon(85)%nucl_atomic_mass = 242.05954
    NuclDataCommon(86)%nucl_atomic_mass = 243.06137
    NuclDataCommon(87)%nucl_atomic_mass = 242.05882
    NuclDataCommon(88)%nucl_atomic_mass = 243.06139
    NuclDataCommon(89)%nucl_atomic_mass = 244.06274
    NuclDataCommon(90)%nucl_atomic_mass = 245.06548
    NuclDataCommon(91)%nucl_atomic_mass = 246.06721
    NuclDataCommon(92)%nucl_atomic_mass = 248.07234
    NuclDataCommon(93)%nucl_atomic_mass = 234.00999
    NuclDataCommon(94)%nucl_atomic_mass = 238.03999
    NuclDataCommon(95)%nucl_atomic_mass = 234.00999
    NuclDataCommon(96)%nucl_atomic_mass = 0.00000
    NuclDataCommon(97)%nucl_atomic_mass = 0.00000
    NuclDataCommon(98)%nucl_atomic_mass = 0.00000
    NuclDataCommon(99)%nucl_atomic_mass = 1.00797
end subroutine FormJARFRisotopicTable