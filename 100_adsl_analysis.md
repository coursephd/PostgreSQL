---
title: "Met RMSD analysis"
author: "Girish Tillu, Ashwini Mathur, Vinay Mahajan"
date: "16 May 2018"
output:
  word_document:
    keep_md: yes
    toc: yes
  pdf_document:
    toc: yes
  html_document:
    df_print: paged
    toc: yes
---






## R Markdown

Frequency counts for patients and summary statistics
<http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


Disease category      Number of patients
-------------------  -------------------
RMSD                               12959
Metabolic                           3114
Metabolic and RMSD                  1333



Disease category      Females   Males
-------------------  --------  ------
Metabolic                1343    1771
Metabolic and RMSD        755     578
RMSD                     7180    5778



Table: All Metabolic patients

| Metabolic|
|---------:|
|      4447|



Table: All RMSD patients

|  RMSD|
|-----:|
| 14292|



Table: Metabolic: Summary statistics in days and visits

get       get.1        n    mean   median       SD   min    max
--------  -------  -----  ------  -------  -------  ----  -----
cdur      -Total    4447   268.9       31   461.38     1   2506
cdur      F         2098   265.2       34   448.15     1   2506
cdur      M         2349   272.1       30   472.95     1   2506
all_ip    -Total    4447     6.6        5     5.82     1     60
all_ip    F         2098     6.3        5     5.04     1     39
all_ip    M         2349     6.9        5     6.55     1     60
all_op    -Total    4447     5.3        2    10.72     1    318
all_op    F         2098     5.3        2     8.86     1    230
all_op    M         2349     5.4        2    12.13     1    318
all_vis   -Total    4447     6.3        3    11.73     1    323
all_vis   F         2098     6.3        3     9.79     1    234
all_vis   M         2349     6.3        2    13.22     1    323



Table: RMSD: Summary statistics in days and visits

get       get.1         n    mean   median       SD   min    max
--------  -------  ------  ------  -------  -------  ----  -----
cdur                    1   526.0      526       NA   526    526
cdur      -Total    14292   228.8       16   436.00     1   2530
cdur      F          7935   234.0       21   431.44     1   2528
cdur      M          6356   222.3       10   441.60     1   2530
all_ip                  1    32.0       32       NA    32     32
all_ip    -Total    14292     7.2        6     6.22     1     86
all_ip    F          7935     7.0        6     5.49     1     86
all_ip    M          6356     7.5        6     7.02     1     83
all_op                  1     2.0        2       NA     2      2
all_op    -Total    14292     4.6        2     9.19     1    318
all_op    F          7935     4.8        2     8.96     1    274
all_op    M          6356     4.3        2     9.47     1    318
all_vis                 1    34.0       34       NA    34     34
all_vis   -Total    14292     5.7        2    10.41     1    323
all_vis   F          7935     5.9        2    10.03     1    274
all_vis   M          6356     5.5        2    10.86     1    323



Table: Diseases: Summary statistics in days, by gender

get      get.1                                                                                  n    mean   median       SD   min    max
-------  ---------------------------------------------------------------------------------  -----  ------  -------  -------  ----  -----
-Total   Metabolic   M10.0   Medoroga                                                         149    32.4      1.0    89.58     1    725
F        Metabolic   M10.0   Medoroga                                                          86    23.1      1.0    61.46     1    303
M        Metabolic   M10.0   Medoroga                                                          63    45.3      1.0   116.98     1    725
-Total   Metabolic   M10.1   Medoroga - Sthula medho roga                                     627    58.7      1.0   190.48     1   1625
F        Metabolic   M10.1   Medoroga - Sthula medho roga                                     402    57.1      1.0   190.70     1   1625
M        Metabolic   M10.1   Medoroga - Sthula medho roga                                     225    61.5      1.0   190.48     1   1343
-Total   Metabolic   M10.2   Medoroga - Sukshma medho roga                                    193    55.7      1.0   145.89     1   1426
F        Metabolic   M10.2   Medoroga - Sukshma medho roga                                     50    37.8      1.0    77.64     1    420
M        Metabolic   M10.2   Medoroga - Sukshma medho roga                                    143    61.9      1.0   162.93     1   1426
-Total   Metabolic   M2.0   Madhumeha                                                        1441   115.4      6.0   235.06     1   1947
F        Metabolic   M2.0   Madhumeha                                                         534   116.3     13.0   235.84     1   1687
M        Metabolic   M2.0   Madhumeha                                                         907   114.9      3.0   234.72     1   1947
-Total   Metabolic   P5.0   Prameha                                                          1497   111.5      1.0   253.01     1   1675
F        Metabolic   P5.0   Prameha                                                           648   116.1      1.0   256.26     1   1569
M        Metabolic   P5.0   Prameha                                                           849   108.0      1.0   250.60     1   1675
-Total   Metabolic   P5.1   Prameha - Krusha                                                    9     2.3      1.0     4.00     1     13
F        Metabolic   P5.1   Prameha - Krusha                                                    4     4.0      1.0     6.00     1     13
M        Metabolic   P5.1   Prameha - Krusha                                                    5     1.0      1.0     0.00     1      1
-Total   Metabolic   P5.2   Prameha - Pidaka                                                   17    10.1      1.0    28.71     1    119
F        Metabolic   P5.2   Prameha - Pidaka                                                    5     5.0      1.0     8.94     1     21
M        Metabolic   P5.2   Prameha - Pidaka                                                   12    12.2      1.0    33.96     1    119
-Total   Metabolic   P5.3   Prameha - Sthula                                                   43    66.6      1.0   180.98     1    822
F        Metabolic   P5.3   Prameha - Sthula                                                   23    12.2      1.0    28.04     1    128
M        Metabolic   P5.3   Prameha - Sthula                                                   20   129.2      1.0   252.57     1    822
-Total   Metabolic   P5.4   Prameha - Upadrava                                                150    42.2      1.0   142.19     1   1180
F        Metabolic   P5.4   Prameha - Upadrava                                                 57    45.4      1.0   123.64     1    562
M        Metabolic   P5.4   Prameha - Upadrava                                                 93    40.3      1.0   153.07     1   1180
-Total   Metabolic   S16.0   Sthaulya                                                         989    42.3      1.0   137.91     1   1883
F        Metabolic   S16.0   Sthaulya                                                         585    48.3      1.0   156.12     1   1883
M        Metabolic   S16.0   Sthaulya                                                         404    33.6      1.0   105.74     1    948
-Total   OTHER   A1.0   Aamadosha                                                              29    40.9      1.0   115.30     1    590
F        OTHER   A1.0   Aamadosha                                                              24    47.1      1.0   126.16     1    590
M        OTHER   A1.0   Aamadosha                                                               5    11.0      1.0    14.14     1     31
-Total   OTHER   A10.0   Arochaka                                                               1    12.0     12.0       NA    12     12
F        OTHER   A10.0   Arochaka                                                               1    12.0     12.0       NA    12     12
-Total   OTHER   A11.0   Arshas                                                               121    69.5      2.0   179.86     1   1125
F        OTHER   A11.0   Arshas                                                                50    89.2      2.0   216.89     1   1125
M        OTHER   A11.0   Arshas                                                                71    55.6      3.0   148.50     1   1057
-Total   OTHER   A11.1   Arsha - Charmakeela                                                    5    43.8      1.0    65.39     1    149
F        OTHER   A11.1   Arsha - Charmakeela                                                    1    67.0     67.0       NA    67     67
M        OTHER   A11.1   Arsha - Charmakeela                                                    4    38.0      1.0    74.00     1    149
-Total   OTHER   A11.10   Arsha - Raktaja Arsha                                                 7    15.0      9.0    24.05     1     68
F        OTHER   A11.10   Arsha - Raktaja Arsha                                                 2    34.5     34.5    47.38     1     68
M        OTHER   A11.10   Arsha - Raktaja Arsha                                                 5     7.2      9.0     6.34     1     16
-Total   OTHER   A11.16   Arsha - Vaataja Arsha                                                 1     1.0      1.0       NA     1      1
M        OTHER   A11.16   Arsha - Vaataja Arsha                                                 1     1.0      1.0       NA     1      1
-Total   OTHER   A11.17   Arsha - Vaatapittaja Arsha                                            1     1.0      1.0       NA     1      1
F        OTHER   A11.17   Arsha - Vaatapittaja Arsha                                            1     1.0      1.0       NA     1      1
-Total   OTHER   A11.18   Arsha - Yonija Arsha                                                  3   127.7    162.0   113.47     1    220
F        OTHER   A11.18   Arsha - Yonija Arsha                                                  3   127.7    162.0   113.47     1    220
-Total   OTHER   A11.2   Arsha - Kapha Pittaja Arsha                                            1     1.0      1.0       NA     1      1
F        OTHER   A11.2   Arsha - Kapha Pittaja Arsha                                            1     1.0      1.0       NA     1      1
-Total   OTHER   A11.3   Arsha - Kaphaja Arsha                                                  1    14.0     14.0       NA    14     14
F        OTHER   A11.3   Arsha - Kaphaja Arsha                                                  1    14.0     14.0       NA    14     14
-Total   OTHER   A11.4   Ashmaree - Sharkaraa                                                   1     1.0      1.0       NA     1      1
F        OTHER   A11.4   Ashmaree - Sharkaraa                                                   1     1.0      1.0       NA     1      1
-Total   OTHER   A11.8   Arsha - Pitta Raktaja Arsha                                            1     1.0      1.0       NA     1      1
M        OTHER   A11.8   Arsha - Pitta Raktaja Arsha                                            1     1.0      1.0       NA     1      1
-Total   OTHER   A11.9   Arsha - Pittaja Arsha                                                  1     1.0      1.0       NA     1      1
M        OTHER   A11.9   Arsha - Pittaja Arsha                                                  1     1.0      1.0       NA     1      1
-Total   OTHER   A12.0   Artava Vyapad                                                        104    66.5      1.0   170.64     1   1506
F        OTHER   A12.0   Artava Vyapad                                                        104    66.5      1.0   170.64     1   1506
-Total   OTHER   A12.1   Artava Vyapad - Anartava                                              23    20.0      1.0    34.35     1    118
F        OTHER   A12.1   Artava Vyapad - Anartava                                              23    20.0      1.0    34.35     1    118
-Total   OTHER   A12.10   Artava Vyapad - Artava Kshaya                                        23    44.0      1.0    82.45     1    284
F        OTHER   A12.10   Artava Vyapad - Artava Kshaya                                        23    44.0      1.0    82.45     1    284
-Total   OTHER   A12.11   ARTAVA VYAPAD - Pushpaghni Jataharini                                17    70.5     27.0    96.91     1    305
F        OTHER   A12.11   ARTAVA VYAPAD - Pushpaghni Jataharini                                17    70.5     27.0    96.91     1    305
-Total   OTHER   A12.2   Artava Vyapad - Granthibhuta                                           1     1.0      1.0       NA     1      1
F        OTHER   A12.2   Artava Vyapad - Granthibhuta                                           1     1.0      1.0       NA     1      1
-Total   OTHER   A12.3   Artava Vyapad - Kaphaja                                                1   183.0    183.0       NA   183    183
F        OTHER   A12.3   Artava Vyapad - Kaphaja                                                1   183.0    183.0       NA   183    183
-Total   OTHER   A12.4   Artava Vyapad - Ksheenartava                                           3    58.0     11.0    90.21     1    162
F        OTHER   A12.4   Artava Vyapad - Ksheenartava                                           3    58.0     11.0    90.21     1    162
-Total   OTHER   A12.5   Artava Vyapad - Kunapagandhi                                           1    10.0     10.0       NA    10     10
F        OTHER   A12.5   Artava Vyapad - Kunapagandhi                                           1    10.0     10.0       NA    10     10
-Total   OTHER   A12.6   Artava Vyapad - Mutra Pureesha gandhi                                  1     1.0      1.0       NA     1      1
M        OTHER   A12.6   Artava Vyapad - Mutra Pureesha gandhi                                  1     1.0      1.0       NA     1      1
-Total   OTHER   A12.7   Artava Vyapad - Pittaja                                                1     1.0      1.0       NA     1      1
F        OTHER   A12.7   Artava Vyapad - Pittaja                                                1     1.0      1.0       NA     1      1
-Total   OTHER   A12.9   Artava Vyapad - Vataja                                                 2     3.5      3.5     3.54     1      6
F        OTHER   A12.9   Artava Vyapad - Vataja                                                 2     3.5      3.5     3.54     1      6
-Total   OTHER   A13.0   Ashmaree                                                              74    60.9      1.0   160.52     1    785
F        OTHER   A13.0   Ashmaree                                                              33    74.8      1.0   197.00     1    785
M        OTHER   A13.0   Ashmaree                                                              41    49.7      1.0   125.25     1    565
-Total   OTHER   A13.2   Ashmaree - Pittaja                                                    18    86.1      9.0   163.73     1    564
F        OTHER   A13.2   Ashmaree - Pittaja                                                    10    85.5     13.5   145.53     1    393
M        OTHER   A13.2   Ashmaree - Pittaja                                                     8    86.9      3.5   194.60     1    564
-Total   OTHER   A13.3   Ashmaree - Pittaja - Shoola                                            4     2.2      1.0     2.50     1      6
F        OTHER   A13.3   Ashmaree - Pittaja - Shoola                                            4     2.2      1.0     2.50     1      6
-Total   OTHER   A14.0   Asragdhara                                                             2    27.0     27.0    36.77     1     53
F        OTHER   A14.0   Asragdhara                                                             2    27.0     27.0    36.77     1     53
-Total   OTHER   A15.0   Atatvaabhinivesha                                                      3     1.0      1.0     0.00     1      1
F        OTHER   A15.0   Atatvaabhinivesha                                                      3     1.0      1.0     0.00     1      1
-Total   OTHER   A16.0   Atisaara                                                              47    40.4      1.0   170.81     1   1070
F        OTHER   A16.0   Atisaara                                                              28    25.4      1.0    96.24     1    503
M        OTHER   A16.0   Atisaara                                                              19    62.6      1.0   244.53     1   1070
-Total   OTHER   A16.1   Atisaara- kapha                                                        1     1.0      1.0       NA     1      1
M        OTHER   A16.1   Atisaara- kapha                                                        1     1.0      1.0       NA     1      1
-Total   OTHER   A16.2   Atisaara- pitta                                                        3     8.7      1.0    13.28     1     24
F        OTHER   A16.2   Atisaara- pitta                                                        2    12.5     12.5    16.26     1     24
M        OTHER   A16.2   Atisaara- pitta                                                        1     1.0      1.0       NA     1      1
-Total   OTHER   A16.6   Atisaara-Vata                                                          1    21.0     21.0       NA    21     21
F        OTHER   A16.6   Atisaara-Vata                                                          1    21.0     21.0       NA    21     21
-Total   OTHER   A17.0   Aatopa                                                                 3     1.0      1.0     0.00     1      1
F        OTHER   A17.0   Aatopa                                                                 2     1.0      1.0     0.00     1      1
M        OTHER   A17.0   Aatopa                                                                 1     1.0      1.0       NA     1      1
-Total   OTHER   A4.0   Agnimaandya                                                            28    29.6      1.0    78.92     1    404
F        OTHER   A4.0   Agnimaandya                                                            19    38.9      1.0    94.73     1    404
M        OTHER   A4.0   Agnimaandya                                                             9    10.0      1.0    13.78     1     33
-Total   OTHER   A5.0   Ajeerna                                                                73    40.2      1.0   145.18     1    986
F        OTHER   A5.0   Ajeerna                                                                38    53.8      1.0   190.67     1    986
M        OTHER   A5.0   Ajeerna                                                                35    25.4      1.0    68.13     1    380
-Total   OTHER   A5.1   Ajeerna- vidagdhajeerna                                                 5   130.2      1.0   220.87     1    511
F        OTHER   A5.1   Ajeerna- vidagdhajeerna                                                 3    46.3      1.0    78.52     1    137
M        OTHER   A5.1   Ajeerna- vidagdhajeerna                                                 2   256.0    256.0   360.62     1    511
-Total   OTHER   A5.2   Ajeerna-Amajeerna                                                       5     1.0      1.0     0.00     1      1
F        OTHER   A5.2   Ajeerna-Amajeerna                                                       1     1.0      1.0       NA     1      1
M        OTHER   A5.2   Ajeerna-Amajeerna                                                       4     1.0      1.0     0.00     1      1
-Total   OTHER   A5.3   Ajeerna-vishtabdhajeerna                                                6     7.2      1.0    15.11     1     38
F        OTHER   A5.3   Ajeerna-vishtabdhajeerna                                                3    13.3      1.0    21.36     1     38
M        OTHER   A5.3   Ajeerna-vishtabdhajeerna                                                3     1.0      1.0     0.00     1      1
-Total   OTHER   A6.0   Amlapitta                                                             565   112.2      1.0   277.90     1   1758
F        OTHER   A6.0   Amlapitta                                                             304   117.1      1.0   279.36     1   1638
M        OTHER   A6.0   Amlapitta                                                             261   106.4      1.0   276.62     1   1758
-Total   OTHER   A6.1   Amlapitta-Adhoga                                                        9     1.8      1.0     2.33     1      8
F        OTHER   A6.1   Amlapitta-Adhoga                                                        6     1.0      1.0     0.00     1      1
M        OTHER   A6.1   Amlapitta-Adhoga                                                        3     3.3      1.0     4.04     1      8
-Total   OTHER   A6.2   Amlapitta-Urdhwaga                                                     79    64.2      1.0   232.07     1   1602
F        OTHER   A6.2   Amlapitta-Urdhwaga                                                     40    77.8     19.5   208.53     1   1284
M        OTHER   A6.2   Amlapitta-Urdhwaga                                                     39    50.2      1.0   255.98     1   1602
-Total   OTHER   A7.0   Anaha                                                                  36    60.2      1.0   178.45     1    838
F        OTHER   A7.0   Anaha                                                                  16    57.9      1.0   174.47     1    702
M        OTHER   A7.0   Anaha                                                                  20    62.1      1.0   186.06     1    838
-Total   OTHER   A7A.0   Anidra                                                                34    46.4      4.0   130.32     1    611
F        OTHER   A7A.0   Anidra                                                                21    50.5      4.0   139.50     1    611
M        OTHER   A7A.0   Anidra                                                                13    39.7      1.0   119.11     1    435
-Total   OTHER   A8.0   Apasmaara                                                              26   122.7     23.0   221.73     1   1002
F        OTHER   A8.0   Apasmaara                                                              11   140.3     42.0   174.34     1    477
M        OTHER   A8.0   Apasmaara                                                              15   109.7     20.0   256.24     1   1002
-Total   OTHER   A8.4   Apasmaara - Kaphaja                                                     1     1.0      1.0       NA     1      1
F        OTHER   A8.4   Apasmaara - Kaphaja                                                     1     1.0      1.0       NA     1      1
-Total   OTHER   A8.7   Apasmaara - Vaataja                                                     1     1.0      1.0       NA     1      1
M        OTHER   A8.7   Apasmaara - Vaataja                                                     1     1.0      1.0       NA     1      1
-Total   OTHER   A9.0   Arbuda                                                                 42    47.6     11.5    91.15     1    455
F        OTHER   A9.0   Arbuda                                                                 27    66.0     30.0   109.09     1    455
M        OTHER   A9.0   Arbuda                                                                 15    14.5      1.0    20.27     1     59
-Total   OTHER   A9.1   Arbuda - Kaphaja                                                        3    25.0     28.0    22.65     1     46
F        OTHER   A9.1   Arbuda - Kaphaja                                                        3    25.0     28.0    22.65     1     46
-Total   OTHER   A9.2   Arbuda - Maamsaja                                                      13    63.8     11.0   141.56     1    522
F        OTHER   A9.2   Arbuda - Maamsaja                                                      12    69.0     16.5   146.53     1    522
M        OTHER   A9.2   Arbuda - Maamsaja                                                       1     1.0      1.0       NA     1      1
-Total   OTHER   A9.3   Arbuda - Medoja Arbuda                                                  2    21.0     21.0    18.38     8     34
F        OTHER   A9.3   Arbuda - Medoja Arbuda                                                  1    34.0     34.0       NA    34     34
M        OTHER   A9.3   Arbuda - Medoja Arbuda                                                  1     8.0      8.0       NA     8      8
-Total   OTHER   A9.4   Arbuda - Medoja Arbuda - Shuddha Vrana                                  1    21.0     21.0       NA    21     21
F        OTHER   A9.4   Arbuda - Medoja Arbuda - Shuddha Vrana                                  1    21.0     21.0       NA    21     21
-Total   OTHER   Anidra   A7A.0                                                                11    87.0     14.0   230.95     1    780
F        OTHER   Anidra   A7A.0                                                                 6   140.2     15.0   313.63     1    780
M        OTHER   Anidra   A7A.0                                                                 5    23.2     11.0    32.58     1     80
-Total   OTHER   B1.0   Baala Roga                                                              6   114.8      1.0   176.74     1    361
F        OTHER   B1.0   Baala Roga                                                              3     1.0      1.0     0.00     1      1
M        OTHER   B1.0   Baala Roga                                                              3   228.7    324.0   198.03     1    361
-Total   OTHER   B1.14   Baala Roga - Baala Shosha                                              1    64.0     64.0       NA    64     64
M        OTHER   B1.14   Baala Roga - Baala Shosha                                              1    64.0     64.0       NA    64     64
-Total   OTHER   B1.2   Baala Roga -  Buddhi Mandya – Manamandata                             1    22.0     22.0       NA    22     22
M        OTHER   B1.2   Baala Roga -  Buddhi Mandya – Manamandata                             1    22.0     22.0       NA    22     22
-Total   OTHER   B1.3   Baala Roga -  Galashundika                                              2     1.0      1.0     0.00     1      1
F        OTHER   B1.3   Baala Roga -  Galashundika                                              1     1.0      1.0       NA     1      1
M        OTHER   B1.3   Baala Roga -  Galashundika                                              1     1.0      1.0       NA     1      1
-Total   OTHER   B1.32   Baala Roga – Hridroga                                                1     1.0      1.0       NA     1      1
M        OTHER   B1.32   Baala Roga – Hridroga                                                1     1.0      1.0       NA     1      1
-Total   OTHER   B1.34   Baala Roga - Krimi Roga                                                7    54.9      1.0    95.50     1    234
F        OTHER   B1.34   Baala Roga - Krimi Roga                                                4    95.2     73.0   114.74     1    234
M        OTHER   B1.34   Baala Roga - Krimi Roga                                                3     1.0      1.0     0.00     1      1
-Total   OTHER   B1.38   Baala Roga – sahaja Vikara                                           1     1.0      1.0       NA     1      1
F        OTHER   B1.38   Baala Roga – sahaja Vikara                                           1     1.0      1.0       NA     1      1
-Total   OTHER   B1.40   Baala Roga - Udarashoola                                               1     1.0      1.0       NA     1      1
M        OTHER   B1.40   Baala Roga - Udarashoola                                               1     1.0      1.0       NA     1      1
-Total   OTHER   B1.9   Baala Roga -  Samvardana Vikara                                        12   184.6     45.5   244.16     1    641
F        OTHER   B1.9   Baala Roga -  Samvardana Vikara                                         5   149.4     51.0   203.68     1    499
M        OTHER   B1.9   Baala Roga -  Samvardana Vikara                                         7   209.7     18.0   282.62     1    641
-Total   OTHER   B2.0   Bhagandara                                                             20    58.9      1.5   192.11     1    863
F        OTHER   B2.0   Bhagandara                                                              3     6.3      2.0     8.39     1     16
M        OTHER   B2.0   Bhagandara                                                             17    68.1      1.0   207.87     1    863
-Total   OTHER   B2.1   Bhagandara - Arsho Bhagandara                                           5    26.0     25.0    25.46     1     56
F        OTHER   B2.1   Bhagandara - Arsho Bhagandara                                           4    26.2     24.0    29.39     1     56
M        OTHER   B2.1   Bhagandara - Arsho Bhagandara                                           1    25.0     25.0       NA    25     25
-Total   OTHER   B2.3   Bhagandara - Parisraavee                                                1     1.0      1.0       NA     1      1
M        OTHER   B2.3   Bhagandara - Parisraavee                                                1     1.0      1.0       NA     1      1
-Total   OTHER   B3.0   Bhagna                                                                118    41.5      2.5    87.69     1    467
F        OTHER   B3.0   Bhagna                                                                 64    32.4      1.0    62.47     1    268
M        OTHER   B3.0   Bhagna                                                                 54    52.4      7.0   110.06     1    467
-Total   OTHER   B3.1.2   Bhagna - Kaanda Bhagna - Asthichhallaka Kaanda Bhagna                 2    11.0     11.0    14.14     1     21
F        OTHER   B3.1.2   Bhagna - Kaanda Bhagna - Asthichhallaka Kaanda Bhagna                 2    11.0     11.0    14.14     1     21
-Total   OTHER   B3.2.0   Bhagna - Sandhimukta Bhagna                                           5     1.4      1.0     0.89     1      3
F        OTHER   B3.2.0   Bhagna - Sandhimukta Bhagna                                           2     1.0      1.0     0.00     1      1
M        OTHER   B3.2.0   Bhagna - Sandhimukta Bhagna                                           3     1.7      1.0     1.15     1      3
-Total   OTHER   B4   Bhasmaka                                                                  1     1.0      1.0       NA     1      1
M        OTHER   B4   Bhasmaka                                                                  1     1.0      1.0       NA     1      1
-Total   OTHER   B5   Bhrama                                                                   74    74.5      2.5   194.23     1    995
F        OTHER   B5   Bhrama                                                                   41    46.5      2.0   159.36     1    995
M        OTHER   B5   Bhrama                                                                   33   109.4      6.0   228.16     1    850
-Total   OTHER   C1.0   Chhardi                                                                 4     2.2      1.0     2.50     1      6
F        OTHER   C1.0   Chhardi                                                                 3     2.7      1.0     2.89     1      6
M        OTHER   C1.0   Chhardi                                                                 1     1.0      1.0       NA     1      1
-Total   OTHER   D0.0   Daaha                                                                   8   152.8      1.0   391.85     1   1121
F        OTHER   D0.0   Daaha                                                                   3    20.3      1.0    33.49     1     59
M        OTHER   D0.0   Daaha                                                                   5   232.2      1.0   497.10     1   1121
-Total   OTHER   D0.1   Daaha - Sthaanabhedena                                                  5    14.2      1.0    29.52     1     67
F        OTHER   D0.1   Daaha - Sthaanabhedena                                                  3     1.0      1.0     0.00     1      1
M        OTHER   D0.1   Daaha - Sthaanabhedena                                                  2    34.0     34.0    46.67     1     67
-Total   OTHER   D0.10   Daaha - Sthaanabhedena - Sarvaanga Daaha                               1    26.0     26.0       NA    26     26
M        OTHER   D0.10   Daaha - Sthaanabhedena - Sarvaanga Daaha                               1    26.0     26.0       NA    26     26
-Total   OTHER   D0.11   Daaha - Sthaanabhedena - Tvak Daaha                                    1    28.0     28.0       NA    28     28
M        OTHER   D0.11   Daaha - Sthaanabhedena - Tvak Daaha                                    1    28.0     28.0       NA    28     28
-Total   OTHER   D0.8   Daaha - Sthaanabhedena - Paada Daaha                                    6    41.5      1.0    98.23     1    242
F        OTHER   D0.8   Daaha - Sthaanabhedena - Paada Daaha                                    2     2.0      2.0     1.41     1      3
M        OTHER   D0.8   Daaha - Sthaanabhedena - Paada Daaha                                    4    61.2      1.0   120.50     1    242
-Total   OTHER   D1.0   Dourbalya                                                              62    47.6      1.0   134.43     1    771
F        OTHER   D1.0   Dourbalya                                                              31    24.0      1.0    41.11     1    171
M        OTHER   D1.0   Dourbalya                                                              31    71.2      1.0   184.14     1    771
-Total   OTHER   G1.6   Garbhapoorva Paricharya                                                 3     1.7      1.0     1.15     1      3
F        OTHER   G1.6   Garbhapoorva Paricharya                                                 3     1.7      1.0     1.15     1      3
-Total   OTHER   G10.0   Guda Roga                                                             12    28.1      1.0    63.67     1    217
F        OTHER   G10.0   Guda Roga                                                              6    54.8     15.0    84.85     1    217
M        OTHER   G10.0   Guda Roga                                                              6     1.3      1.0     0.82     1      3
-Total   OTHER   G13.0   Gulma                                                                 20    79.1      1.0   288.69     1   1301
F        OTHER   G13.0   Gulma                                                                  8     6.9      1.0     9.49     1     22
M        OTHER   G13.0   Gulma                                                                 12   127.2      2.0   370.91     1   1301
-Total   OTHER   G3.0   Garbhapata                                                              2    34.5     34.5    47.38     1     68
F        OTHER   G3.0   Garbhapata                                                              2    34.5     34.5    47.38     1     68
-Total   OTHER   G4.0   Garbhasrava                                                             1     1.0      1.0       NA     1      1
F        OTHER   G4.0   Garbhasrava                                                             1     1.0      1.0       NA     1      1
-Total   OTHER   G5.0   Garbhavrudhi                                                            1   255.0    255.0       NA   255    255
F        OTHER   G5.0   Garbhavrudhi                                                            1   255.0    255.0       NA   255    255
-Total   OTHER   G6.0   Garbhini                                                               26   126.6      3.0   302.08     1   1275
F        OTHER   G6.0   Garbhini                                                               26   126.6      3.0   302.08     1   1275
-Total   OTHER   G6.1   Garbhini - chardi                                                       1     3.0      3.0       NA     3      3
F        OTHER   G6.1   Garbhini - chardi                                                       1     3.0      3.0       NA     3      3
-Total   OTHER   G6.2   Garbhini - jwara                                                        1     1.0      1.0       NA     1      1
F        OTHER   G6.2   Garbhini - jwara                                                        1     1.0      1.0       NA     1      1
-Total   OTHER   G6.4   Garbhini - pandu                                                        1     1.0      1.0       NA     1      1
F        OTHER   G6.4   Garbhini - pandu                                                        1     1.0      1.0       NA     1      1
-Total   OTHER   G8.0   Grahanee                                                               60   106.0      1.0   244.92     1   1029
F        OTHER   G8.0   Grahanee                                                               32   102.0      3.0   203.33     1    805
M        OTHER   G8.0   Grahanee                                                               28   110.6      1.0   289.09     1   1029
-Total   OTHER   G8.1   Grahanee-kaphaj                                                         1     1.0      1.0       NA     1      1
M        OTHER   G8.1   Grahanee-kaphaj                                                         1     1.0      1.0       NA     1      1
-Total   OTHER   G8.2   Grahanee-pittaj                                                         1   469.0    469.0       NA   469    469
F        OTHER   G8.2   Grahanee-pittaj                                                         1   469.0    469.0       NA   469    469
-Total   OTHER   G8.5   Grahanee-Vataj                                                          6     6.8      1.0    14.29     1     36
F        OTHER   G8.5   Grahanee-Vataj                                                          3     1.0      1.0     0.00     1      1
M        OTHER   G8.5   Grahanee-Vataj                                                          3    12.7      1.0    20.21     1     36
-Total   OTHER   G9.0   Granthi                                                                45    51.3      1.0   102.55     1    398
F        OTHER   G9.0   Granthi                                                                30    39.0      1.0    86.04     1    344
M        OTHER   G9.0   Granthi                                                                15    75.9      1.0   129.39     1    398
-Total   OTHER   G9.1   Granthi - Asthi Granthi                                                 7    81.3     23.0   144.19     1    398
F        OTHER   G9.1   Granthi - Asthi Granthi                                                 4   132.0     64.5   183.01     1    398
M        OTHER   G9.1   Granthi - Asthi Granthi                                                 3    13.7     16.0    11.68     1     24
-Total   OTHER   G9.10   Granthi (Garbhashaya)                                                  6   208.5      2.0   496.59     1   1222
F        OTHER   G9.10   Granthi (Garbhashaya)                                                  6   208.5      2.0   496.59     1   1222
-Total   OTHER   G9.11   GRANTHI - BEEJAKOSHA                                                   5   107.2     74.0   132.48     1    322
F        OTHER   G9.11   GRANTHI - BEEJAKOSHA                                                   5   107.2     74.0   132.48     1    322
-Total   OTHER   G9.2   Granthi - Kaphaja Granthi                                               2   246.0    246.0   346.48     1    491
F        OTHER   G9.2   Granthi - Kaphaja Granthi                                               2   246.0    246.0   346.48     1    491
-Total   OTHER   G9.3   Granthi - Maamsaja Granthi                                             12    71.2      8.0   127.97     1    342
F        OTHER   G9.3   Granthi - Maamsaja Granthi                                              6   124.3     32.5   167.99     1    342
M        OTHER   G9.3   Granthi - Maamsaja Granthi                                              6    18.0      7.0    32.02     1     83
-Total   OTHER   G9.4   Granthi - Medoja Granthi                                               21    27.1      1.0    61.20     1    259
F        OTHER   G9.4   Granthi - Medoja Granthi                                               11    47.7     22.0    80.36     1    259
M        OTHER   G9.4   Granthi - Medoja Granthi                                               10     4.5      1.0     7.82     1     24
-Total   OTHER   G9.6   Granthi - Raktaja Granthi                                               3    73.7      1.0   125.86     1    219
F        OTHER   G9.6   Granthi - Raktaja Granthi                                               3    73.7      1.0   125.86     1    219
-Total   OTHER   G9.7   Granthi - Siraa Granthi                                               213   103.5      5.0   227.56     1   1508
F        OTHER   G9.7   Granthi - Siraa Granthi                                               153   104.5      6.0   229.50     1   1508
M        OTHER   G9.7   Granthi - Siraa Granthi                                                60   100.8      1.0   224.40     1    987
-Total   OTHER   G9.8   Granthi - Vaataja Granthi                                               1     1.0      1.0       NA     1      1
F        OTHER   G9.8   Granthi - Vaataja Granthi                                               1     1.0      1.0       NA     1      1
-Total   OTHER   G9.9   Granthi - Vranaja Granthi                                               4    15.0      3.0    25.40     1     53
M        OTHER   G9.9   Granthi - Vranaja Granthi                                               4    15.0      3.0    25.40     1     53
-Total   OTHER   H1.0   Hikkaa                                                                  2     4.5      4.5     4.95     1      8
M        OTHER   H1.0   Hikkaa                                                                  2     4.5      4.5     4.95     1      8
-Total   OTHER   H2.0   Hridroga                                                              136    64.3      1.0   190.22     1   1517
F        OTHER   H2.0   Hridroga                                                               45    63.2      1.0   192.34     1   1098
M        OTHER   H2.0   Hridroga                                                               91    64.8      1.0   190.23     1   1517
-Total   OTHER   H2.1   Hridroga - Kaphaja Hridroga                                             1     1.0      1.0       NA     1      1
M        OTHER   H2.1   Hridroga - Kaphaja Hridroga                                             1     1.0      1.0       NA     1      1
-Total   OTHER   H2.3   Hridroga - Pittaja Hridroga                                             4     1.0      1.0     0.00     1      1
F        OTHER   H2.3   Hridroga - Pittaja Hridroga                                             2     1.0      1.0     0.00     1      1
M        OTHER   H2.3   Hridroga - Pittaja Hridroga                                             2     1.0      1.0     0.00     1      1
-Total   OTHER   H2.4   Hridroga - Saannipaatika Hridroga                                       3    71.3      1.0   121.82     1    212
F        OTHER   H2.4   Hridroga - Saannipaatika Hridroga                                       1     1.0      1.0       NA     1      1
M        OTHER   H2.4   Hridroga - Saannipaatika Hridroga                                       2   106.5    106.5   149.20     1    212
-Total   OTHER   H2.5   Hridroga - Vaataja Hridroga                                             4    67.5      1.0   133.00     1    267
F        OTHER   H2.5   Hridroga - Vaataja Hridroga                                             2     1.0      1.0     0.00     1      1
M        OTHER   H2.5   Hridroga - Vaataja Hridroga                                             2   134.0    134.0   188.09     1    267
-Total   OTHER   J1.0   Jwara                                                                 113    49.6      1.0   178.83     1   1359
F        OTHER   J1.0   Jwara                                                                  64    49.4      1.0   150.29     1    701
M        OTHER   J1.0   Jwara                                                                  49    49.8      1.0   212.07     1   1359
-Total   OTHER   J1.1   Jvara- kapha                                                            4     1.0      1.0     0.00     1      1
F        OTHER   J1.1   Jvara- kapha                                                            4     1.0      1.0     0.00     1      1
-Total   OTHER   J1.2   Jvara- pitta                                                            4     1.0      1.0     0.00     1      1
F        OTHER   J1.2   Jvara- pitta                                                            3     1.0      1.0     0.00     1      1
M        OTHER   J1.2   Jvara- pitta                                                            1     1.0      1.0       NA     1      1
-Total   OTHER   J1.4   Jvara-sannipata                                                         1     1.0      1.0       NA     1      1
F        OTHER   J1.4   Jvara-sannipata                                                         1     1.0      1.0       NA     1      1
-Total   OTHER   J1.5   Jvara-vata                                                              2     2.5      2.5     2.12     1      4
F        OTHER   J1.5   Jvara-vata                                                              1     1.0      1.0       NA     1      1
M        OTHER   J1.5   Jvara-vata                                                              1     4.0      4.0       NA     4      4
-Total   OTHER   J1.6   Jvara-vishama                                                          12    13.3      1.0    25.31     1     78
F        OTHER   J1.6   Jvara-vishama                                                           9    17.4      1.0    28.36     1     78
M        OTHER   J1.6   Jvara-vishama                                                           3     1.0      1.0     0.00     1      1
-Total   OTHER   K1.0   Kaamalaa                                                               16    65.6      4.5   125.15     1    427
F        OTHER   K1.0   Kaamalaa                                                                3     3.3      1.0     4.04     1      8
M        OTHER   K1.0   Kaamalaa                                                               13    80.0     12.0   135.58     1    427
-Total   OTHER   K10.10   Kshudra Roga - Charmakeela                                            5    22.8      1.0    32.31     1     73
F        OTHER   K10.10   Kshudra Roga - Charmakeela                                            2    55.5     55.5    24.75    38     73
M        OTHER   K10.10   Kshudra Roga - Charmakeela                                            3     1.0      1.0     0.00     1      1
-Total   OTHER   K10.11   Kshudra Roga - Chippa                                                 2     1.0      1.0     0.00     1      1
F        OTHER   K10.11   Kshudra Roga - Chippa                                                 1     1.0      1.0       NA     1      1
M        OTHER   K10.11   Kshudra Roga - Chippa                                                 1     1.0      1.0       NA     1      1
-Total   OTHER   K10.15   Kshudra Roga - Indralupta                                             5     1.0      1.0     0.00     1      1
F        OTHER   K10.15   Kshudra Roga - Indralupta                                             2     1.0      1.0     0.00     1      1
M        OTHER   K10.15   Kshudra Roga - Indralupta                                             3     1.0      1.0     0.00     1      1
-Total   OTHER   K10.23   Kshudra Roga - Kadara                                                20     6.3      1.0    10.68     1     34
F        OTHER   K10.23   Kshudra Roga - Kadara                                                17     5.4      1.0     9.47     1     34
M        OTHER   K10.23   Kshudra Roga - Kadara                                                 3    11.3      1.0    17.90     1     32
-Total   OTHER   K10.27   Kshudra Roga - Kotha                                                  1    90.0     90.0       NA    90     90
M        OTHER   K10.27   Kshudra Roga - Kotha                                                  1    90.0     90.0       NA    90     90
-Total   OTHER   K10.28   Kshudra Roga - Kunakha                                               16    30.3      1.0    54.60     1    178
F        OTHER   K10.28   Kshudra Roga - Kunakha                                                8    37.4      1.0    62.39     1    178
M        OTHER   K10.28   Kshudra Roga - Kunakha                                                8    23.2      1.0    48.82     1    139
-Total   OTHER   K10.29   Kshudra Roga - Khalitya                                              58    31.9      1.0   107.79     1    650
F        OTHER   K10.29   Kshudra Roga - Khalitya                                              42    24.4      1.0    79.75     1    489
M        OTHER   K10.29   Kshudra Roga - Khalitya                                              16    51.6      1.0   161.86     1    650
-Total   OTHER   K10.32   Kshudra Roga - Mukhadooshikaa                                        11    24.0      1.0    32.10     1     82
F        OTHER   K10.32   Kshudra Roga - Mukhadooshikaa                                        10    26.3     11.0    32.86     1     82
M        OTHER   K10.32   Kshudra Roga - Mukhadooshikaa                                         1     1.0      1.0       NA     1      1
-Total   OTHER   K10.34   Kshudra Roga - Niruddhaprakasha                                       1     1.0      1.0       NA     1      1
M        OTHER   K10.34   Kshudra Roga - Niruddhaprakasha                                       1     1.0      1.0       NA     1      1
-Total   OTHER   K10.36   Kshudra Roga - Paadadaarikaa                                          2     1.0      1.0     0.00     1      1
F        OTHER   K10.36   Kshudra Roga - Paadadaarikaa                                          2     1.0      1.0     0.00     1      1
-Total   OTHER   K10.37.3   Kshudra Roga - Padminikantaka                                       1     1.0      1.0       NA     1      1
M        OTHER   K10.37.3   Kshudra Roga - Padminikantaka                                       1     1.0      1.0       NA     1      1
-Total   OTHER   K10.38   Kshudra Roga - Parivartikaa                                           4     4.8      1.0     7.50     1     16
M        OTHER   K10.38   Kshudra Roga - Parivartikaa                                           4     4.8      1.0     7.50     1     16
-Total   OTHER   K10.38A   Kshudra Roga - Parikartikaa                                         86    65.4      6.5   144.26     1    720
F        OTHER   K10.38A   Kshudra Roga - Parikartikaa                                         46    59.7      4.5   139.52     1    720
M        OTHER   K10.38A   Kshudra Roga - Parikartikaa                                         40    71.9     12.0   151.05     1    582
-Total   OTHER   K10.38B   Kshudra Roga - Parivartikaa                                          1    36.0     36.0       NA    36     36
M        OTHER   K10.38B   Kshudra Roga - Parivartikaa                                          1    36.0     36.0       NA    36     36
-Total   OTHER   K10.43   Kshudra Roga - Sanniruddha Guda                                       1     1.0      1.0       NA     1      1
F        OTHER   K10.43   Kshudra Roga - Sanniruddha Guda                                       1     1.0      1.0       NA     1      1
-Total   OTHER   K10.46.0   Kshudra Roga - Valmeeka                                             1     1.0      1.0       NA     1      1
M        OTHER   K10.46.0   Kshudra Roga - Valmeeka                                             1     1.0      1.0       NA     1      1
-Total   OTHER   K10.51   Kshudra Roga - Vrishana Kachchhoo                                     1     1.0      1.0       NA     1      1
M        OTHER   K10.51   Kshudra Roga - Vrishana Kachchhoo                                     1     1.0      1.0       NA     1      1
-Total   OTHER   K10.52.0   Kshudra Roga - Vyanga                                               8    29.0      1.0    75.24     1    215
F        OTHER   K10.52.0   Kshudra Roga - Vyanga                                               8    29.0      1.0    75.24     1    215
-Total   OTHER   K10.52.5   Kshudra Roga - Yavaprakhyaa                                         3     7.0      1.0    10.39     1     19
F        OTHER   K10.52.5   Kshudra Roga - Yavaprakhyaa                                         2    10.0     10.0    12.73     1     19
M        OTHER   K10.52.5   Kshudra Roga - Yavaprakhyaa                                         1     1.0      1.0       NA     1      1
-Total   OTHER   K10.52.8   Kshudra Roga - Vaatakantaka                                        32    44.7      1.0   129.99     1    712
F        OTHER   K10.52.8   Kshudra Roga - Vaatakantaka                                        24    47.9      1.0   146.01     1    712
M        OTHER   K10.52.8   Kshudra Roga - Vaatakantaka                                         8    35.2      1.0    68.19     1    194
-Total   OTHER   K10.8   Kshudra Roga - Arumshikaa                                              3    16.3      1.0    26.56     1     47
F        OTHER   K10.8   Kshudra Roga - Arumshikaa                                              3    16.3      1.0    26.56     1     47
-Total   OTHER   K2.0   Kaasa                                                                 314    77.4      1.0   192.00     1   1303
F        OTHER   K2.0   Kaasa                                                                 200    62.6      1.0   150.18     1   1265
M        OTHER   K2.0   Kaasa                                                                 114   103.3      1.0   247.66     1   1303
-Total   OTHER   K2.1   Kaasa- kapha                                                           31     7.3      1.0    19.30     1     99
F        OTHER   K2.1   Kaasa- kapha                                                           12    11.2      1.0    28.51     1     99
M        OTHER   K2.1   Kaasa- kapha                                                           19     4.9      1.0    10.41     1     42
-Total   OTHER   K2.2   Kaasa- kshataja                                                         2    12.0     12.0     4.24     9     15
F        OTHER   K2.2   Kaasa- kshataja                                                         1     9.0      9.0       NA     9      9
M        OTHER   K2.2   Kaasa- kshataja                                                         1    15.0     15.0       NA    15     15
-Total   OTHER   K2.3   Kaasa- kshayaja                                                         1     1.0      1.0       NA     1      1
M        OTHER   K2.3   Kaasa- kshayaja                                                         1     1.0      1.0       NA     1      1
-Total   OTHER   K2.4   Kaasa- pitta                                                            1     1.0      1.0       NA     1      1
M        OTHER   K2.4   Kaasa- pitta                                                            1     1.0      1.0       NA     1      1
-Total   OTHER   K2.5   Kaasa- vata                                                            20   161.7     14.0   378.19     1   1526
F        OTHER   K2.5   Kaasa- vata                                                            12   260.9     39.5   468.99     1   1526
M        OTHER   K2.5   Kaasa- vata                                                             8    12.9      1.0    20.16     1     58
-Total   OTHER   K3.0   Kandoo                                                                 63    35.6      1.0    80.54     1    418
F        OTHER   K3.0   Kandoo                                                                 38    33.1      1.0    84.63     1    418
M        OTHER   K3.0   Kandoo                                                                 25    39.4      1.0    75.43     1    269
-Total   OTHER   K3.1   Kandoo - Guda Kandoo                                                    5     4.2      1.0     6.61     1     16
F        OTHER   K3.1   Kandoo - Guda Kandoo                                                    2     1.0      1.0     0.00     1      1
M        OTHER   K3.1   Kandoo - Guda Kandoo                                                    3     6.3      2.0     8.39     1     16
-Total   OTHER   K4.0   Karna Roga                                                             32    21.4      1.0    52.41     1    221
F        OTHER   K4.0   Karna Roga                                                             18    19.9      1.0    51.27     1    221
M        OTHER   K4.0   Karna Roga                                                             14    23.3      1.0    55.73     1    208
-Total   OTHER   K4.1   Karna Roga - Karna   Pooyasraava                                        6    12.3      1.0    23.63     1     60
F        OTHER   K4.1   Karna Roga - Karna   Pooyasraava                                        4    18.0      5.5    28.32     1     60
M        OTHER   K4.1   Karna Roga - Karna   Pooyasraava                                        2     1.0      1.0     0.00     1      1
-Total   OTHER   K4.10   Karna Roga - Karnabaadhirya - Kaphaanubandha                          12    28.0      3.0    49.39     1    169
F        OTHER   K4.10   Karna Roga - Karnabaadhirya - Kaphaanubandha                           7    43.6     16.0    61.05     1    169
M        OTHER   K4.10   Karna Roga - Karnabaadhirya - Kaphaanubandha                           5     6.2      1.0    10.03     1     24
-Total   OTHER   K4.11   Karna Roga - Karnabaadhirya - Shuddha Vaataja                         11    32.8      1.0    74.82     1    249
F        OTHER   K4.11   Karna Roga - Karnabaadhirya - Shuddha Vaataja                          6    54.5      1.5    99.51     1    249
M        OTHER   K4.11   Karna Roga - Karnabaadhirya - Shuddha Vaataja                          5     6.8      1.0     8.01     1     17
-Total   OTHER   K4.12   Karna Roga - Karnagoothaka                                             7     8.1      1.0    18.46     1     50
F        OTHER   K4.12   Karna Roga - Karnagoothaka                                             2     1.0      1.0     0.00     1      1
M        OTHER   K4.12   Karna Roga - Karnagoothaka                                             5    11.0      1.0    21.81     1     50
-Total   OTHER   K4.13   Karna Roga - Karnakandoo                                               8    15.5      7.5    19.02     1     53
F        OTHER   K4.13   Karna Roga - Karnakandoo                                               3    25.3     22.0    26.16     1     53
M        OTHER   K4.13   Karna Roga - Karnakandoo                                               5     9.6      1.0    13.22     1     31
-Total   OTHER   K4.14   Karna Roga - Karnakshveda                                             16    40.1      1.0    74.98     1    254
F        OTHER   K4.14   Karna Roga - Karnakshveda                                             10    33.8      1.0    59.67     1    157
M        OTHER   K4.14   Karna Roga - Karnakshveda                                              6    50.7      1.0   101.23     1    254
-Total   OTHER   K4.16   Karna Roga - Karnapaaka                                                1     2.0      2.0       NA     2      2
F        OTHER   K4.16   Karna Roga - Karnapaaka                                                1     2.0      2.0       NA     2      2
-Total   OTHER   K4.18   Karna Roga - Karnapratinaaha                                           1    11.0     11.0       NA    11     11
F        OTHER   K4.18   Karna Roga - Karnapratinaaha                                           1    11.0     11.0       NA    11     11
-Total   OTHER   K4.19   Karna Roga - Karnashoola                                              13    56.4      1.0   167.71     1    610
F        OTHER   K4.19   Karna Roga - Karnashoola                                               9    78.6      1.0   200.87     1    610
M        OTHER   K4.19   Karna Roga - Karnashoola                                               4     6.5      1.0    11.00     1     23
-Total   OTHER   K4.2   Karna Roga - Karna  Kleda                                               1     1.0      1.0       NA     1      1
F        OTHER   K4.2   Karna Roga - Karna  Kleda                                               1     1.0      1.0       NA     1      1
-Total   OTHER   K4.20   Karna Roga - Karnashopha                                               4     1.0      1.0     0.00     1      1
F        OTHER   K4.20   Karna Roga - Karnashopha                                               2     1.0      1.0     0.00     1      1
M        OTHER   K4.20   Karna Roga - Karnashopha                                               2     1.0      1.0     0.00     1      1
-Total   OTHER   K4.21   Karna Roga - Karnasraava                                               9    12.7      1.0    26.43     1     79
F        OTHER   K4.21   Karna Roga - Karnasraava                                               8    14.1      1.0    27.86     1     79
M        OTHER   K4.21   Karna Roga - Karnasraava                                               1     1.0      1.0       NA     1      1
-Total   OTHER   K4.24   Karna Roga - Pootikarnaka                                              1    25.0     25.0       NA    25     25
M        OTHER   K4.24   Karna Roga - Pootikarnaka                                              1    25.0     25.0       NA    25     25
-Total   OTHER   K4.25   Karna Roga - Uchcha Shruti                                             1    18.0     18.0       NA    18     18
F        OTHER   K4.25   Karna Roga - Uchcha Shruti                                             1    18.0     18.0       NA    18     18
-Total   OTHER   K4.3   Karna Roga - Karna  Supti                                               1     1.0      1.0       NA     1      1
M        OTHER   K4.3   Karna Roga - Karna  Supti                                               1     1.0      1.0       NA     1      1
-Total   OTHER   K4.6   Karna Roga - Karna Vidradhi                                             1     1.0      1.0       NA     1      1
F        OTHER   K4.6   Karna Roga - Karna Vidradhi                                             1     1.0      1.0       NA     1      1
         OTHER   K4.9   Karna Roga - Karnabaadhirya                                             1    25.0     25.0       NA    25     25
-Total   OTHER   K4.9   Karna Roga - Karnabaadhirya                                            13    76.1     13.0   134.13     1    385
F        OTHER   K4.9   Karna Roga - Karnabaadhirya                                             6   156.8    122.0   168.95     1    385
M        OTHER   K4.9   Karna Roga - Karnabaadhirya                                             6     3.8      1.0     4.92     1     13
-Total   OTHER   K5.0   Khalitya                                                                1     1.0      1.0       NA     1      1
M        OTHER   K5.0   Khalitya                                                                1     1.0      1.0       NA     1      1
-Total   OTHER   K6.0   Klaibya                                                                12    14.9      1.0    35.80     1    122
M        OTHER   K6.0   Klaibya                                                                12    14.9      1.0    35.80     1    122
-Total   OTHER   K6.14   Klaibya - Shukrakshayaja                                               1     8.0      8.0       NA     8      8
M        OTHER   K6.14   Klaibya - Shukrakshayaja                                               1     8.0      8.0       NA     8      8
-Total   OTHER   K6.2   Klaibya - Beejopaghaataja                                               2    45.0     45.0    38.18    18     72
M        OTHER   K6.2   Klaibya - Beejopaghaataja                                               2    45.0     45.0    38.18    18     72
-Total   OTHER   K6.3   Klaibya - Dhaatukshayaja                                                1     1.0      1.0       NA     1      1
M        OTHER   K6.3   Klaibya - Dhaatukshayaja                                                1     1.0      1.0       NA     1      1
-Total   OTHER   K6.4   Klaibya - Dvajabhangaja                                                 4     1.0      1.0     0.00     1      1
M        OTHER   K6.4   Klaibya - Dvajabhangaja                                                 4     1.0      1.0     0.00     1      1
-Total   OTHER   K7.0   Klama                                                                  18    12.1      1.0    22.89     1     83
F        OTHER   K7.0   Klama                                                                  11    16.0      1.0    28.31     1     83
M        OTHER   K7.0   Klama                                                                   7     6.0      1.0     8.77     1     22
-Total   OTHER   K7a.0   Kotha                                                                  4    24.0     18.5    15.90    12     47
F        OTHER   K7a.0   Kotha                                                                  2    17.0     17.0     7.07    12     22
M        OTHER   K7a.0   Kotha                                                                  2    31.0     31.0    22.63    15     47
-Total   OTHER   K9.0   Kshudra Roga                                                          165    55.2      1.0   160.84     1   1307
F        OTHER   K9.0   Kshudra Roga                                                           89    54.5      1.0   125.26     1    726
M        OTHER   K9.0   Kshudra Roga                                                           76    56.1      1.0   195.34     1   1307
-Total   OTHER   K9.1   Kshudra Roga - Agnirohinee                                              1     1.0      1.0       NA     1      1
F        OTHER   K9.1   Kshudra Roga - Agnirohinee                                              1     1.0      1.0       NA     1      1
-Total   OTHER   K9.4   Kshudra Roga - Alajee                                                   7   140.0     52.0   187.72     1    442
F        OTHER   K9.4   Kshudra Roga - Alajee                                                   5   176.8     52.0   214.08     1    442
M        OTHER   K9.4   Kshudra Roga - Alajee                                                   2    48.0     48.0    66.47     1     95
-Total   OTHER   K9.5   Kshudra Roga - Alasa                                                    1    41.0     41.0       NA    41     41
M        OTHER   K9.5   Kshudra Roga - Alasa                                                    1    41.0     41.0       NA    41     41
-Total   OTHER   M1   Madaatyaya                                                                4    12.2     10.5    11.18     1     27
M        OTHER   M1   Madaatyaya                                                                4    12.2     10.5    11.18     1     27
-Total   OTHER   M1.0   Madaatyaya - Paramada                                                  12    43.2      1.0   108.74     1    379
F        OTHER   M1.0   Madaatyaya - Paramada                                                   1    90.0     90.0       NA    90     90
M        OTHER   M1.0   Madaatyaya - Paramada                                                  11    38.9      1.0   112.99     1    379
-Total   OTHER   M1.1   Madaatyaya - Pittaja                                                    4    60.5      1.5   118.33     1    238
M        OTHER   M1.1   Madaatyaya - Pittaja                                                    4    60.5      1.5   118.33     1    238
-Total   OTHER   M1.4   Madaatyaya - Vaataja                                                    2     1.0      1.0     0.00     1      1
M        OTHER   M1.4   Madaatyaya - Vaataja                                                    2     1.0      1.0     0.00     1      1
-Total   OTHER   M1.6   Madaatyaya - Vikshaya                                                   1     1.0      1.0       NA     1      1
M        OTHER   M1.6   Madaatyaya - Vikshaya                                                   1     1.0      1.0       NA     1      1
-Total   OTHER   M12.0   Mootra Dosha                                                          11    37.5      1.0    71.06     1    182
F        OTHER   M12.0   Mootra Dosha                                                           9    45.7      1.0    76.84     1    182
M        OTHER   M12.0   Mootra Dosha                                                           2     1.0      1.0     0.00     1      1
-Total   OTHER   M13.0   Mootra Roga                                                           68    55.0      1.0   144.18     1    997
F        OTHER   M13.0   Mootra Roga                                                           43    58.3      1.0   166.98     1    997
M        OTHER   M13.0   Mootra Roga                                                           25    49.4      1.0    95.86     1    356
-Total   OTHER   M14.0   Mootra Roga - Mootraaghaata                                            2    23.5     23.5    30.41     2     45
F        OTHER   M14.0   Mootra Roga - Mootraaghaata                                            1     2.0      2.0       NA     2      2
M        OTHER   M14.0   Mootra Roga - Mootraaghaata                                            1    45.0     45.0       NA    45     45
-Total   OTHER   M14.13   Mootra Roga - Mootraaghaata - Raktagranthi                            1    36.0     36.0       NA    36     36
M        OTHER   M14.13   Mootra Roga - Mootraaghaata - Raktagranthi                            1    36.0     36.0       NA    36     36
-Total   OTHER   M14.14   Mootra Roga - Mootraaghaata - Ushnavaata                              5     4.8      1.0     8.50     1     20
F        OTHER   M14.14   Mootra Roga - Mootraaghaata - Ushnavaata                              2     1.0      1.0     0.00     1      1
M        OTHER   M14.14   Mootra Roga - Mootraaghaata - Ushnavaata                              3     7.3      1.0    10.97     1     20
-Total   OTHER   M14.15   Mootra Roga - Mootraaghaata - Vaataashthilaa                          3     1.0      1.0     0.00     1      1
M        OTHER   M14.15   Mootra Roga - Mootraaghaata - Vaataashthilaa                          3     1.0      1.0     0.00     1      1
-Total   OTHER   M14.16   Mootra Roga - Mootraaghaata - Vaatabasti                              1    26.0     26.0       NA    26     26
M        OTHER   M14.16   Mootra Roga - Mootraaghaata - Vaatabasti                              1    26.0     26.0       NA    26     26
-Total   OTHER   M14.5   Mootra Roga - Mootraaghaata - Mootragranthi                            2     8.0      8.0     8.49     2     14
M        OTHER   M14.5   Mootra Roga - Mootraaghaata - Mootragranthi                            2     8.0      8.0     8.49     2     14
-Total   OTHER   M16.0   Mootra Roga - Mootrakrichchhra                                        49    37.4      1.0   100.61     1    643
F        OTHER   M16.0   Mootra Roga - Mootrakrichchhra                                        39    44.5      1.0   111.74     1    643
M        OTHER   M16.0   Mootra Roga - Mootrakrichchhra                                        10     9.6      1.0    13.95     1     42
-Total   OTHER   M16.2   Mootra Roga - Mootrakrichchhra - Kaphaja                               2     4.0      4.0     4.24     1      7
F        OTHER   M16.2   Mootra Roga - Mootrakrichchhra - Kaphaja                               2     4.0      4.0     4.24     1      7
-Total   OTHER   M16.9   Mootra Roga - Mootrakrichchhra - Vaataja Mootrakrichchhra              3    85.0     36.0   116.50     1    218
F        OTHER   M16.9   Mootra Roga - Mootrakrichchhra - Vaataja Mootrakrichchhra              1     1.0      1.0       NA     1      1
M        OTHER   M16.9   Mootra Roga - Mootrakrichchhra - Vaataja Mootrakrichchhra              2   127.0    127.0   128.69    36    218
-Total   OTHER   M18.0   Mukha Roga  - Mukha Shosha                                             4   180.0     14.5   340.24     1    690
F        OTHER   M18.0   Mukha Roga  - Mukha Shosha                                             2    14.5     14.5    19.09     1     28
M        OTHER   M18.0   Mukha Roga  - Mukha Shosha                                             2   345.5    345.5   487.20     1    690
-Total   OTHER   M18.10.0   Mukha Roga - Dantamoolagata Roga                                    2     1.0      1.0     0.00     1      1
M        OTHER   M18.10.0   Mukha Roga - Dantamoolagata Roga                                    2     1.0      1.0     0.00     1      1
-Total   OTHER   M18.10.10   Mukha Roga - Dantamoolagata Roga - Sheetaada                       6    29.7     11.5    49.85     1    129
F        OTHER   M18.10.10   Mukha Roga - Dantamoolagata Roga - Sheetaada                       2     1.0      1.0     0.00     1      1
M        OTHER   M18.10.10   Mukha Roga - Dantamoolagata Roga - Sheetaada                       4    44.0     23.0    57.61     1    129
-Total   OTHER   M18.10.12   Mukha Roga - Dantamoolagata Roga - Upakusha                        2    27.0     27.0    12.73    18     36
F        OTHER   M18.10.12   Mukha Roga - Dantamoolagata Roga - Upakusha                        1    18.0     18.0       NA    18     18
M        OTHER   M18.10.12   Mukha Roga - Dantamoolagata Roga - Upakusha                        1    36.0     36.0       NA    36     36
-Total   OTHER   M18.10.3   Mukha Roga - Dantamoolagata Roga - Dantapupputa                     2    28.0     28.0    12.73    19     37
F        OTHER   M18.10.3   Mukha Roga - Dantamoolagata Roga - Dantapupputa                     2    28.0     28.0    12.73    19     37
-Total   OTHER   M18.10.8   Mukha Roga - Dantamoolagata Roga - Mahaasushira                     1    41.0     41.0       NA    41     41
F        OTHER   M18.10.8   Mukha Roga - Dantamoolagata Roga - Mahaasushira                     1    41.0     41.0       NA    41     41
-Total   OTHER   M18.11.0   Mukha Roga - Galagata Roga                                         51    26.3      1.0    56.67     1    246
F        OTHER   M18.11.0   Mukha Roga - Galagata Roga                                         37    29.6      1.0    61.62     1    246
M        OTHER   M18.11.0   Mukha Roga - Galagata Roga                                         14    17.6      1.0    41.51     1    138
-Total   OTHER   M18.11.10   Mukha Roga - Galagata Roga - Gandamaalaa                           5     4.4      1.0     7.60     1     18
F        OTHER   M18.11.10   Mukha Roga - Galagata Roga - Gandamaalaa                           5     4.4      1.0     7.60     1     18
-Total   OTHER   M18.11.2   Mukha Roga - Galagata Roga - Galaganda                             66    79.9      1.0   234.78     1   1547
F        OTHER   M18.11.2   Mukha Roga - Galagata Roga - Galaganda                             52    39.8      1.0    94.05     1    601
M        OTHER   M18.11.2   Mukha Roga - Galagata Roga - Galaganda                             14   228.5      6.0   459.02     1   1547
-Total   OTHER   M18.11.4   Mukha Roga - Galagata Roga - Galapaaka                              9     3.2      1.0     5.93     1     19
F        OTHER   M18.11.4   Mukha Roga - Galagata Roga - Galapaaka                              6     1.3      1.0     0.52     1      2
M        OTHER   M18.11.4   Mukha Roga - Galagata Roga - Galapaaka                              3     7.0      1.0    10.39     1     19
-Total   OTHER   M18.11.6   Mukha Roga - Galagata Roga - Galashundika                           1    27.0     27.0       NA    27     27
F        OTHER   M18.11.6   Mukha Roga - Galagata Roga - Galashundika                           1    27.0     27.0       NA    27     27
-Total   OTHER   M18.11.8   Mukha Roga - Galagata Roga - Shaalooka                              1    14.0     14.0       NA    14     14
F        OTHER   M18.11.8   Mukha Roga - Galagata Roga - Shaalooka                              1    14.0     14.0       NA    14     14
-Total   OTHER   M18.11.9   Mukha Roga - Galagata Roga - Tundikeree                             5    21.6      9.0    27.09     1     64
F        OTHER   M18.11.9   Mukha Roga - Galagata Roga - Tundikeree                             1     9.0      9.0       NA     9      9
M        OTHER   M18.11.9   Mukha Roga - Galagata Roga - Tundikeree                             4    24.8     17.0    30.20     1     64
-Total   OTHER   M18.12.0   Mukha Roga - Gandagata Roga                                         3   180.0    165.0    42.53   147    228
F        OTHER   M18.12.0   Mukha Roga - Gandagata Roga                                         3   180.0    165.0    42.53   147    228
-Total   OTHER   M18.13.2   Mukha Roga - Jihvaagata Roga  - Jihvaa Paaka                        2    10.5     10.5    13.44     1     20
F        OTHER   M18.13.2   Mukha Roga - Jihvaagata Roga  - Jihvaa Paaka                        1     1.0      1.0       NA     1      1
M        OTHER   M18.13.2   Mukha Roga - Jihvaagata Roga  - Jihvaa Paaka                        1    20.0     20.0       NA    20     20
-Total   OTHER   M18.14.0   Mukha Roga - Kanthagata Roga                                       19    15.8      1.0    29.65     1    114
F        OTHER   M18.14.0   Mukha Roga - Kanthagata Roga                                       14    21.1      1.0    33.20     1    114
M        OTHER   M18.14.0   Mukha Roga - Kanthagata Roga                                        5     1.0      1.0     0.00     1      1
-Total   OTHER   M18.14.11   Mukha Roga - Kanthagata Roga - Galavidradhi                        1     1.0      1.0       NA     1      1
F        OTHER   M18.14.11   Mukha Roga - Kanthagata Roga - Galavidradhi                        1     1.0      1.0       NA     1      1
-Total   OTHER   M18.14.17   Mukha Roga - Kanthagata Roga - Svaraghna                           2    19.0     19.0    25.46     1     37
F        OTHER   M18.14.17   Mukha Roga - Kanthagata Roga - Svaraghna                           1     1.0      1.0       NA     1      1
M        OTHER   M18.14.17   Mukha Roga - Kanthagata Roga - Svaraghna                           1    37.0     37.0       NA    37     37
-Total   OTHER   M18.14.18   Mukha Roga - Kanthagata Roga - Tundeekeree                         1     1.0      1.0       NA     1      1
F        OTHER   M18.14.18   Mukha Roga - Kanthagata Roga - Tundeekeree                         1     1.0      1.0       NA     1      1
-Total   OTHER   M18.15.0   Mukha Roga - Kaphaja                                                2     1.0      1.0     0.00     1      1
F        OTHER   M18.15.0   Mukha Roga - Kaphaja                                                1     1.0      1.0       NA     1      1
M        OTHER   M18.15.0   Mukha Roga - Kaphaja                                                1     1.0      1.0       NA     1      1
-Total   OTHER   M18.16.8   Mukha Roga - Oshthagata Roga - Oshthakopa                           1     1.0      1.0       NA     1      1
F        OTHER   M18.16.8   Mukha Roga - Oshthagata Roga - Oshthakopa                           1     1.0      1.0       NA     1      1
-Total   OTHER   M18.19.4   Mukha Roga - Sarvagata - Mukhapaaka                                 8    11.4      1.0    29.34     1     84
F        OTHER   M18.19.4   Mukha Roga - Sarvagata - Mukhapaaka                                 3     1.0      1.0     0.00     1      1
M        OTHER   M18.19.4   Mukha Roga - Sarvagata - Mukhapaaka                                 5    17.6      1.0    37.12     1     84
-Total   OTHER   M18.2   Mukha Roga  - Mukha Vairasya                                           1     1.0      1.0       NA     1      1
M        OTHER   M18.2   Mukha Roga  - Mukha Vairasya                                           1     1.0      1.0       NA     1      1
-Total   OTHER   M18.3   Mukha Roga  - Mukhadaaha                                               4    41.2     40.0    45.97     1     84
F        OTHER   M18.3   Mukha Roga  - Mukhadaaha                                               1    78.0     78.0       NA    78     78
M        OTHER   M18.3   Mukha Roga  - Mukhadaaha                                               3    29.0      2.0    47.63     1     84
-Total   OTHER   M18.9.0   Mukha Roga - Dantagata Roga                                         25    16.2      1.0    43.10     1    181
F        OTHER   M18.9.0   Mukha Roga - Dantagata Roga                                         16    22.7      1.5    53.15     1    181
M        OTHER   M18.9.0   Mukha Roga - Dantagata Roga                                          9     4.6      1.0     6.33     1     17
-Total   OTHER   M18.9.1   Mukha Roga - Dantagata Roga  - Danta Daurbalya                      17    12.4      1.0    23.21     1     72
F        OTHER   M18.9.1   Mukha Roga - Dantagata Roga  - Danta Daurbalya                       7    10.7      1.0    22.37     1     61
M        OTHER   M18.9.1   Mukha Roga - Dantagata Roga  - Danta Daurbalya                      10    13.6      1.0    24.90     1     72
-Total   OTHER   M18.9.11   Mukha Roga - Dantagata Roga - Bhanjanaka                            1     1.0      1.0       NA     1      1
M        OTHER   M18.9.11   Mukha Roga - Dantagata Roga - Bhanjanaka                            1     1.0      1.0       NA     1      1
-Total   OTHER   M18.9.14   Mukha Roga - Dantagata Roga - Dantabheda                            1     1.0      1.0       NA     1      1
F        OTHER   M18.9.14   Mukha Roga - Dantagata Roga - Dantabheda                            1     1.0      1.0       NA     1      1
-Total   OTHER   M18.9.15   Mukha Roga - Dantagata Roga - Dantaharsha                           2     1.0      1.0     0.00     1      1
F        OTHER   M18.9.15   Mukha Roga - Dantagata Roga - Dantaharsha                           1     1.0      1.0       NA     1      1
M        OTHER   M18.9.15   Mukha Roga - Dantagata Roga - Dantaharsha                           1     1.0      1.0       NA     1      1
-Total   OTHER   M18.9.2   Mukha Roga - Dantagata Roga  - Danta Maamsa Kandoo                  38    11.6      1.0    41.11     1    231
F        OTHER   M18.9.2   Mukha Roga - Dantagata Roga  - Danta Maamsa Kandoo                  16    23.1      1.0    62.24     1    231
M        OTHER   M18.9.2   Mukha Roga - Dantagata Roga  - Danta Maamsa Kandoo                  22     3.2      1.0     5.96     1     21
-Total   OTHER   M18.9.21   Mukha Roga - Dantagata Roga - Krimidanta                            2     2.5      2.5     2.12     1      4
M        OTHER   M18.9.21   Mukha Roga - Dantagata Roga - Krimidanta                            2     2.5      2.5     2.12     1      4
-Total   OTHER   M18.9.25   Mukha Roga - Dantagata Roga - Sheetadanta                           2    18.0     18.0    21.21     3     33
F        OTHER   M18.9.25   Mukha Roga - Dantagata Roga - Sheetadanta                           1    33.0     33.0       NA    33     33
M        OTHER   M18.9.25   Mukha Roga - Dantagata Roga - Sheetadanta                           1     3.0      3.0       NA     3      3
-Total   OTHER   M18.9.3   Mukha Roga - Dantagata Roga  - Danta Maamsa Paaka                   12    17.1      2.5    28.17     1     98
F        OTHER   M18.9.3   Mukha Roga - Dantagata Roga  - Danta Maamsa Paaka                    7    28.1     23.0    33.33     1     98
M        OTHER   M18.9.3   Mukha Roga - Dantagata Roga  - Danta Maamsa Paaka                    5     1.6      1.0     1.34     1      4
-Total   OTHER   M18.9.4   Mukha Roga - Dantagata Roga  - Danta Maamsa Shoola                   4     1.0      1.0     0.00     1      1
F        OTHER   M18.9.4   Mukha Roga - Dantagata Roga  - Danta Maamsa Shoola                   2     1.0      1.0     0.00     1      1
M        OTHER   M18.9.4   Mukha Roga - Dantagata Roga  - Danta Maamsa Shoola                   2     1.0      1.0     0.00     1      1
-Total   OTHER   M18.9.5   Mukha Roga - Dantagata Roga  - Danta Maamsa Sraava                   2     1.0      1.0     0.00     1      1
F        OTHER   M18.9.5   Mukha Roga - Dantagata Roga  - Danta Maamsa Sraava                   1     1.0      1.0       NA     1      1
M        OTHER   M18.9.5   Mukha Roga - Dantagata Roga  - Danta Maamsa Sraava                   1     1.0      1.0       NA     1      1
-Total   OTHER   M18.9.6   Mukha Roga - Dantagata Roga  - Danta Shoola                          2     1.5      1.5     0.71     1      2
F        OTHER   M18.9.6   Mukha Roga - Dantagata Roga  - Danta Shoola                          2     1.5      1.5     0.71     1      2
-Total   OTHER   M18.9.8   Mukha Roga - Dantagata Roga  - Dantagata Raktasraava                 2     1.0      1.0     0.00     1      1
M        OTHER   M18.9.8   Mukha Roga - Dantagata Roga  - Dantagata Raktasraava                 2     1.0      1.0     0.00     1      1
-Total   OTHER   M5.0   Maanasa Roga                                                           35   137.0      1.0   292.33     1   1059
F        OTHER   M5.0   Maanasa Roga                                                           16   118.1      1.0   272.89     1    887
M        OTHER   M5.0   Maanasa Roga                                                           19   152.9      5.0   314.28     1   1059
-Total   OTHER   M5.1   Maanasa Roga - Vishada                                                 34    65.6     14.5   171.41     1    994
F        OTHER   M5.1   Maanasa Roga - Vishada                                                 23    85.8     30.0   205.34     1    994
M        OTHER   M5.1   Maanasa Roga - Vishada                                                 11    23.5      9.0    36.19     1    101
-Total   OTHER   M5.2   Maanasa Roga - Glaani                                                   5     4.2      1.0     4.60     1     11
M        OTHER   M5.2   Maanasa Roga - Glaani                                                   5     4.2      1.0     4.60     1     11
-Total   OTHER   M5.3   Smriti Bhrmsha                                                         19   110.6     14.0   168.70     1    477
F        OTHER   M5.3   Smriti Bhrmsha                                                          3   172.0     38.0   264.78     1    477
M        OTHER   M5.3   Smriti Bhrmsha                                                         16    99.1      7.5   154.62     1    425
-Total   OTHER   M6.0   Marma Viddha                                                           24    32.9      7.0   101.47     1    503
F        OTHER   M6.0   Marma Viddha                                                           11    11.1      7.0    10.45     1     34
M        OTHER   M6.0   Marma Viddha                                                           13    51.4      1.0   137.25     1    503
-Total   OTHER   M6.1   Marma Viddha - Aani Viddha                                              1     1.0      1.0       NA     1      1
M        OTHER   M6.1   Marma Viddha - Aani Viddha                                              1     1.0      1.0       NA     1      1
-Total   OTHER   M6.13   Marma Viddha - Gulpha Viddha                                           3    26.3      1.0    43.88     1     77
F        OTHER   M6.13   Marma Viddha - Gulpha Viddha                                           3    26.3      1.0    43.88     1     77
-Total   OTHER   M6.16   Marma Viddha - Jaanu Viddha                                           11    17.5     10.0    27.37     1     92
F        OTHER   M6.16   Marma Viddha - Jaanu Viddha                                            3    13.7     14.0     3.51    10     17
M        OTHER   M6.16   Marma Viddha - Jaanu Viddha                                            8    19.0      1.0    32.53     1     92
-Total   OTHER   M6.25   Marma Viddha - Kukundara Viddha                                        1    88.0     88.0       NA    88     88
M        OTHER   M6.25   Marma Viddha - Kukundara Viddha                                        1    88.0     88.0       NA    88     88
-Total   OTHER   M6.27   Marma Viddha - Maamsa Viddha                                          23    22.9     13.0    38.70     1    167
F        OTHER   M6.27   Marma Viddha - Maamsa Viddha                                          14    28.9     13.0    46.99     1    167
M        OTHER   M6.27   Marma Viddha - Maamsa Viddha                                           9    13.6      1.0    19.23     1     57
-Total   OTHER   M6.29   Marma Viddha - Manibandha Viddha                                       2     1.0      1.0     0.00     1      1
F        OTHER   M6.29   Marma Viddha - Manibandha Viddha                                       1     1.0      1.0       NA     1      1
M        OTHER   M6.29   Marma Viddha - Manibandha Viddha                                       1     1.0      1.0       NA     1      1
-Total   OTHER   M6.3   Marma Viddha - Amsa Viddha                                              6    28.2      1.0    65.57     1    162
F        OTHER   M6.3   Marma Viddha - Amsa Viddha                                              1     1.0      1.0       NA     1      1
M        OTHER   M6.3   Marma Viddha - Amsa Viddha                                              5    33.6      1.0    71.78     1    162
-Total   OTHER   M6.33   Marma Viddha - Nitamba Viddha                                          1     1.0      1.0       NA     1      1
M        OTHER   M6.33   Marma Viddha - Nitamba Viddha                                          1     1.0      1.0       NA     1      1
-Total   OTHER   M6.38   Marma Viddha - Sandhi Viddha                                          14     9.2      1.0    14.97     1     47
F        OTHER   M6.38   Marma Viddha - Sandhi Viddha                                           6    14.5      4.0    19.49     1     47
M        OTHER   M6.38   Marma Viddha - Sandhi Viddha                                           8     5.2      1.0    10.15     1     30
-Total   OTHER   M6.43   Marma Viddha - Snaayu Viddha                                          51    69.7      3.0   126.90     1    584
F        OTHER   M6.43   Marma Viddha - Snaayu Viddha                                          27    66.2     11.0   112.56     1    459
M        OTHER   M6.43   Marma Viddha - Snaayu Viddha                                          24    73.6      2.0   143.73     1    584
-Total   OTHER   M6.8   Marma Viddha - Asthimarma Viddha                                       12    14.5     11.5    16.83     1     56
F        OTHER   M6.8   Marma Viddha - Asthimarma Viddha                                       11    14.4      9.0    17.65     1     56
M        OTHER   M6.8   Marma Viddha - Asthimarma Viddha                                        1    16.0     16.0       NA    16     16
-Total   OTHER   M7.1   Masanumasika Ashtama                                                    1     1.0      1.0       NA     1      1
F        OTHER   M7.1   Masanumasika Ashtama                                                    1     1.0      1.0       NA     1      1
-Total   OTHER   M7.3   Masanumasika Dviteeya                                                   4     5.0      1.0     8.00     1     17
F        OTHER   M7.3   Masanumasika Dviteeya                                                   4     5.0      1.0     8.00     1     17
-Total   OTHER   M7.5   Masanumasika Panchama                                                   1     1.0      1.0       NA     1      1
F        OTHER   M7.5   Masanumasika Panchama                                                   1     1.0      1.0       NA     1      1
-Total   OTHER   M7.7   Masanumasika Saptama                                                    1     1.0      1.0       NA     1      1
F        OTHER   M7.7   Masanumasika Saptama                                                    1     1.0      1.0       NA     1      1
-Total   OTHER   M8.0   Masoorikaa                                                              1     1.0      1.0       NA     1      1
F        OTHER   M8.0   Masoorikaa                                                              1     1.0      1.0       NA     1      1
-Total   OTHER   M9.0   Medhra Roga                                                             2   244.0    244.0   343.65     1    487
M        OTHER   M9.0   Medhra Roga                                                             2   244.0    244.0   343.65     1    487
-Total   OTHER   MS G1.0   Greeva Graha                                                       619    89.1      4.0   216.42     1   1713
F        OTHER   MS G1.0   Greeva Graha                                                       360    89.0      6.5   200.27     1   1423
M        OTHER   MS G1.0   Greeva Graha                                                       259    89.1      1.0   237.46     1   1713
-Total   OTHER   MS M1.0   Mutrashteela                                                        64    99.1      2.0   245.45     1   1578
F        OTHER   MS M1.0   Mutrashteela                                                         2   118.0    118.0   152.74    10    226
M        OTHER   MS M1.0   Mutrashteela                                                        62    98.5      2.0   248.65     1   1578
-Total   OTHER   MS R1   Raktachapa                                                           149    62.3      1.0   181.83     1   1439
F        OTHER   MS R1   Raktachapa                                                            75    59.8      1.0   198.31     1   1439
M        OTHER   MS R1   Raktachapa                                                            74    64.9      3.0   164.78     1    967
-Total   OTHER   MS S1.0   Shukra Dushti                                                        1     1.0      1.0       NA     1      1
M        OTHER   MS S1.0   Shukra Dushti                                                        1     1.0      1.0       NA     1      1
-Total   OTHER   MS T1.0   Twak Vivarnya                                                       56    32.0      1.0    88.72     1    463
F        OTHER   MS T1.0   Twak Vivarnya                                                       40    23.3      1.0    76.06     1    463
M        OTHER   MS T1.0   Twak Vivarnya                                                       16    53.8      1.0   114.50     1    364
-Total   OTHER   N1.0   Naadee Vrana                                                            8     6.5      1.0    12.96     1     38
F        OTHER   N1.0   Naadee Vrana                                                            2     1.0      1.0     0.00     1      1
M        OTHER   N1.0   Naadee Vrana                                                            6     8.3      1.0    14.80     1     38
-Total   OTHER   N1.1   Naadee Vrana - Kaphaja                                                  1     1.0      1.0       NA     1      1
M        OTHER   N1.1   Naadee Vrana - Kaphaja                                                  1     1.0      1.0       NA     1      1
-Total   OTHER   N1.4   Naadee Vrana - Shalyaja                                                 1   116.0    116.0       NA   116    116
F        OTHER   N1.4   Naadee Vrana - Shalyaja                                                 1   116.0    116.0       NA   116    116
-Total   OTHER   N1.5   Naadee Vrana - Vaataja                                                  1     1.0      1.0       NA     1      1
M        OTHER   N1.5   Naadee Vrana - Vaataja                                                  1     1.0      1.0       NA     1      1
-Total   OTHER   N2.0   Naasaa Roga                                                             6   226.2      3.5   535.49     1   1319
F        OTHER   N2.0   Naasaa Roga                                                             3    12.0      6.0    14.93     1     29
M        OTHER   N2.0   Naasaa Roga                                                             3   440.3      1.0   760.95     1   1319
-Total   OTHER   N2.1   Naasaa Roga -  Naasaavarodha                                           14    28.2      1.0    50.87     1    168
F        OTHER   N2.1   Naasaa Roga -  Naasaavarodha                                            6    25.0      4.5    41.57     1    106
M        OTHER   N2.1   Naasaa Roga -  Naasaavarodha                                            8    30.6      1.0    59.62     1    168
-Total   OTHER   N2.11   Naasaa Roga - Naasaa Sraava                                            1     1.0      1.0       NA     1      1
M        OTHER   N2.11   Naasaa Roga - Naasaa Sraava                                            1     1.0      1.0       NA     1      1
-Total   OTHER   N2.12   Naasaa Roga - Naasaanaaha                                              8     6.9      1.0    10.88     1     25
F        OTHER   N2.12   Naasaa Roga - Naasaanaaha                                              3    16.7     24.0    13.58     1     25
M        OTHER   N2.12   Naasaa Roga - Naasaanaaha                                              5     1.0      1.0     0.00     1      1
-Total   OTHER   N2.16   Naasaa Roga - Naasaashosha                                             1     1.0      1.0       NA     1      1
F        OTHER   N2.16   Naasaa Roga - Naasaashosha                                             1     1.0      1.0       NA     1      1
-Total   OTHER   N2.18   Naasaa Roga - Peenasa                                                155   110.6      1.0   250.47     1   1387
F        OTHER   N2.18   Naasaa Roga - Peenasa                                                 76    91.8      1.0   190.90     1    985
M        OTHER   N2.18   Naasaa Roga - Peenasa                                                 79   128.6      1.0   296.90     1   1387
-Total   OTHER   N2.2   Naasaa Roga - Apeenasa                                                  2     1.0      1.0     0.00     1      1
F        OTHER   N2.2   Naasaa Roga - Apeenasa                                                  2     1.0      1.0     0.00     1      1
-Total   OTHER   N2.21.0   Naasaa Roga - Pratishyaaya                                         142    66.7      1.0   195.49     1   1346
F        OTHER   N2.21.0   Naasaa Roga - Pratishyaaya                                          81    63.1      1.0   209.04     1   1346
M        OTHER   N2.21.0   Naasaa Roga - Pratishyaaya                                          61    71.4      1.0   177.51     1    890
-Total   OTHER   N2.21.1   Naasaa Roga - Pratishyaaya - Dushta Pratishyaaya                    40    71.0      1.0   295.61     1   1765
F        OTHER   N2.21.1   Naasaa Roga - Pratishyaaya - Dushta Pratishyaaya                    21    88.3      1.0   384.27     1   1765
M        OTHER   N2.21.1   Naasaa Roga - Pratishyaaya - Dushta Pratishyaaya                    19    51.7      1.0   156.57     1    683
-Total   OTHER   N2.21.2   Naasaa Roga - Pratishyaaya - Kaphaja Pratishyaaya                   15    13.0      1.0    37.79     1    148
F        OTHER   N2.21.2   Naasaa Roga - Pratishyaaya - Kaphaja Pratishyaaya                    6     6.5      1.0     8.57     1     19
M        OTHER   N2.21.2   Naasaa Roga - Pratishyaaya - Kaphaja Pratishyaaya                    9    17.3      1.0    49.00     1    148
-Total   OTHER   N2.21.3   Naasaa Roga - Pratishyaaya - Pittaja Pratishyaaya                    4    74.2     13.0   130.99     1    270
F        OTHER   N2.21.3   Naasaa Roga - Pratishyaaya - Pittaja Pratishyaaya                    2     1.0      1.0     0.00     1      1
M        OTHER   N2.21.3   Naasaa Roga - Pratishyaaya - Pittaja Pratishyaaya                    2   147.5    147.5   173.24    25    270
-Total   OTHER   N2.21.7   Naasaa Roga - Pratishyaaya - Vaataja Pratishyaaya                   14    81.1      9.0   189.84     1    722
F        OTHER   N2.21.7   Naasaa Roga - Pratishyaaya - Vaataja Pratishyaaya                    5   193.4     57.0   299.42     1    722
M        OTHER   N2.21.7   Naasaa Roga - Pratishyaaya - Vaataja Pratishyaaya                    9    18.8      1.0    38.57     1    119
-Total   OTHER   N2.6   Naasaa Roga - Deepta                                                    1     1.0      1.0       NA     1      1
M        OTHER   N2.6   Naasaa Roga - Deepta                                                    1     1.0      1.0       NA     1      1
-Total   OTHER   N2.9   Naasaa Roga - Kshavathu                                                 6    23.5      8.0    34.94     1     90
F        OTHER   N2.9   Naasaa Roga - Kshavathu                                                 1    15.0     15.0       NA    15     15
M        OTHER   N2.9   Naasaa Roga - Kshavathu                                                 5    25.2      1.0    38.78     1     90
-Total   OTHER   N3.13   Napumsakatva - Shandha                                                 1   257.0    257.0       NA   257    257
M        OTHER   N3.13   Napumsakatva - Shandha                                                 1   257.0    257.0       NA   257    257
-Total   OTHER   N4.0   Netra Roga                                                            430    71.7      1.0   209.31     1   1333
F        OTHER   N4.0   Netra Roga                                                            209    76.3      1.0   222.76     1   1333
M        OTHER   N4.0   Netra Roga                                                            221    67.3      1.0   196.16     1   1234
-Total   OTHER   N4.1   Netra Roga - Abhishyanda                                                5     2.4      1.0     3.13     1      8
F        OTHER   N4.1   Netra Roga - Abhishyanda                                                2     4.5      4.5     4.95     1      8
M        OTHER   N4.1   Netra Roga - Abhishyanda                                                3     1.0      1.0     0.00     1      1
-Total   OTHER   N4.10   Netra Roga - Gharsha                                                   3     1.0      1.0     0.00     1      1
F        OTHER   N4.10   Netra Roga - Gharsha                                                   1     1.0      1.0       NA     1      1
M        OTHER   N4.10   Netra Roga - Gharsha                                                   2     1.0      1.0     0.00     1      1
-Total   OTHER   N4.11   Netra Roga - Jaadya                                                    1     1.0      1.0       NA     1      1
M        OTHER   N4.11   Netra Roga - Jaadya                                                    1     1.0      1.0       NA     1      1
-Total   OTHER   N4.12   Netra Roga - Kandoo                                                   12     5.1      1.0     8.77     1     30
F        OTHER   N4.12   Netra Roga - Kandoo                                                   10     5.9      1.0     9.47     1     30
M        OTHER   N4.12   Netra Roga - Kandoo                                                    2     1.0      1.0     0.00     1      1
-Total   OTHER   N4.13   Netra Roga - Kaphaja Linganaasha Shastrakarma Pashchaat               11    48.1      4.0   121.09     1    408
F        OTHER   N4.13   Netra Roga - Kaphaja Linganaasha Shastrakarma Pashchaat                8     6.5      2.5     9.53     1     29
M        OTHER   N4.13   Netra Roga - Kaphaja Linganaasha Shastrakarma Pashchaat                3   159.0     68.0   218.23     1    408
-Total   OTHER   N4.14   Netra Roga - Upanaaha                                                  1     1.0      1.0       NA     1      1
M        OTHER   N4.14   Netra Roga - Upanaaha                                                  1     1.0      1.0       NA     1      1
-Total   OTHER   N4.17   Netra Roga - Masoorikaa                                                1     1.0      1.0       NA     1      1
F        OTHER   N4.17   Netra Roga - Masoorikaa                                                1     1.0      1.0       NA     1      1
-Total   OTHER   N4.19   Netra Roga - Netra Daaha                                               7    12.7      1.0    22.95     1     62
F        OTHER   N4.19   Netra Roga - Netra Daaha                                               3     1.3      1.0     0.58     1      2
M        OTHER   N4.19   Netra Roga - Netra Daaha                                               4    21.2     11.0    28.76     1     62
-Total   OTHER   N4.20   Netra Roga - Netra Gaurava                                             1    14.0     14.0       NA    14     14
F        OTHER   N4.20   Netra Roga - Netra Gaurava                                             1    14.0     14.0       NA    14     14
-Total   OTHER   N4.22   Netra Roga - Netra Paaka                                               1     1.0      1.0       NA     1      1
M        OTHER   N4.22   Netra Roga - Netra Paaka                                               1     1.0      1.0       NA     1      1
-Total   OTHER   N4.23   Netra Roga - Netra Raajee                                              2     1.0      1.0     0.00     1      1
F        OTHER   N4.23   Netra Roga - Netra Raajee                                              1     1.0      1.0       NA     1      1
M        OTHER   N4.23   Netra Roga - Netra Raajee                                              1     1.0      1.0       NA     1      1
-Total   OTHER   N4.24   Netra Roga - Netra Shoola                                              1    26.0     26.0       NA    26     26
F        OTHER   N4.24   Netra Roga - Netra Shoola                                              1    26.0     26.0       NA    26     26
-Total   OTHER   N4.25   Netra Roga - Netra Sraava                                              7    18.0      2.0    36.39     1     99
F        OTHER   N4.25   Netra Roga - Netra Sraava                                              5    24.6      2.0    42.37     1     99
M        OTHER   N4.25   Netra Roga - Netra Sraava                                              2     1.5      1.5     0.71     1      2
-Total   OTHER   N4.27   Netra Roga - Paichchhilya                                              1   201.0    201.0       NA   201    201
F        OTHER   N4.27   Netra Roga - Paichchhilya                                              1   201.0    201.0       NA   201    201
-Total   OTHER   N4.30   Netra Roga - Sashopha Akshipaaka                                       1     1.0      1.0       NA     1      1
M        OTHER   N4.30   Netra Roga - Sashopha Akshipaaka                                       1     1.0      1.0       NA     1      1
-Total   OTHER   N4.35   Netra Roga - Triteeya Patalagata Timira                               50    34.0      1.0    80.67     1    337
F        OTHER   N4.35   Netra Roga - Triteeya Patalagata Timira                               30    43.7      1.0    84.13     1    323
M        OTHER   N4.35   Netra Roga - Triteeya Patalagata Timira                               20    19.4      1.0    74.89     1    337
-Total   OTHER   N4.38   Netra Roga - Vaataja Abhishyanda + Shoola                              2     1.5      1.5     0.71     1      2
F        OTHER   N4.38   Netra Roga - Vaataja Abhishyanda + Shoola                              2     1.5      1.5     0.71     1      2
-Total   OTHER   N4.4   Netra Roga  - Alpashopha Akshipaaka                                     4    20.8     18.0    13.67     8     39
F        OTHER   N4.4   Netra Roga  - Alpashopha Akshipaaka                                     2    18.0     18.0     7.07    13     23
M        OTHER   N4.4   Netra Roga  - Alpashopha Akshipaaka                                     2    23.5     23.5    21.92     8     39
-Total   OTHER   N4.40.0   Netra Roga - Drishtigata                                             7    13.7      1.0    33.20     1     89
F        OTHER   N4.40.0   Netra Roga - Drishtigata                                             3     1.3      1.0     0.58     1      2
M        OTHER   N4.40.0   Netra Roga - Drishtigata                                             4    23.0      1.0    44.00     1     89
-Total   OTHER   N4.40.10   Netra Roga - Drishtigata - Kaacha Roga - Pittaja                    1     1.0      1.0       NA     1      1
M        OTHER   N4.40.10   Netra Roga - Drishtigata - Kaacha Roga - Pittaja                    1     1.0      1.0       NA     1      1
-Total   OTHER   N4.40.14   Netra Roga - Drishtigata - Kaacha Roga - Vaataja                    1     1.0      1.0       NA     1      1
F        OTHER   N4.40.14   Netra Roga - Drishtigata - Kaacha Roga - Vaataja                    1     1.0      1.0       NA     1      1
-Total   OTHER   N4.40.15   Netra Roga - Drishtigata - Kaphavidagdha Drishti                    1    13.0     13.0       NA    13     13
F        OTHER   N4.40.15   Netra Roga - Drishtigata - Kaphavidagdha Drishti                    1    13.0     13.0       NA    13     13
-Total   OTHER   N4.40.16.0   Netra Roga - Drishtigata - Linganaasha                            2   124.0    124.0   172.53     2    246
F        OTHER   N4.40.16.0   Netra Roga - Drishtigata - Linganaasha                            1   246.0    246.0       NA   246    246
M        OTHER   N4.40.16.0   Netra Roga - Drishtigata - Linganaasha                            1     2.0      2.0       NA     2      2
-Total   OTHER   N4.40.16.3   Netra Roga - Drishtigata - Linganaasha - Kaphaja                  1     3.0      3.0       NA     3      3
M        OTHER   N4.40.16.3   Netra Roga - Drishtigata - Linganaasha - Kaphaja                  1     3.0      3.0       NA     3      3
-Total   OTHER   N4.40.19   Netra Roga - Drishtigata - Pittavidagdha Drishti                    3     1.0      1.0     0.00     1      1
F        OTHER   N4.40.19   Netra Roga - Drishtigata - Pittavidagdha Drishti                    1     1.0      1.0       NA     1      1
M        OTHER   N4.40.19   Netra Roga - Drishtigata - Pittavidagdha Drishti                    2     1.0      1.0     0.00     1      1
-Total   OTHER   N4.40.20.0   Netra Roga - Drishtigata - Timira                                18    16.8      1.0    37.44     1    154
F        OTHER   N4.40.20.0   Netra Roga - Drishtigata - Timira                                 7    34.9      2.0    56.56     1    154
M        OTHER   N4.40.20.0   Netra Roga - Drishtigata - Timira                                11     5.3      1.0     9.43     1     32
-Total   OTHER   N4.40.20.1   Netra Roga - Drishtigata - Timira - Kaphaja Timira               25    13.6      1.0    29.66     1    115
F        OTHER   N4.40.20.1   Netra Roga - Drishtigata - Timira - Kaphaja Timira               12    15.1      1.0    28.81     1     93
M        OTHER   N4.40.20.1   Netra Roga - Drishtigata - Timira - Kaphaja Timira               13    12.2      1.0    31.53     1    115
-Total   OTHER   N4.40.20.3   Netra Roga - Drishtigata - Timira - Raktaja Timira                1    31.0     31.0       NA    31     31
F        OTHER   N4.40.20.3   Netra Roga - Drishtigata - Timira - Raktaja Timira                1    31.0     31.0       NA    31     31
-Total   OTHER   N4.40.20.6   Netra Roga - Drishtigata - Timira - Vaataja Timira                4    76.0      5.0   144.72     1    293
F        OTHER   N4.40.20.6   Netra Roga - Drishtigata - Timira - Vaataja Timira                2     5.0      5.0     5.66     1      9
M        OTHER   N4.40.20.6   Netra Roga - Drishtigata - Timira - Vaataja Timira                2   147.0    147.0   206.48     1    293
-Total   OTHER   N4.40.5   Netra Roga - Drishtigata - Doshaandhya                               2     1.0      1.0     0.00     1      1
F        OTHER   N4.40.5   Netra Roga - Drishtigata - Doshaandhya                               1     1.0      1.0       NA     1      1
M        OTHER   N4.40.5   Netra Roga - Drishtigata - Doshaandhya                               1     1.0      1.0       NA     1      1
-Total   OTHER   N4.40.6   Netra Roga - Drishtigata - Gambheera Drishti                         1    21.0     21.0       NA    21     21
F        OTHER   N4.40.6   Netra Roga - Drishtigata - Gambheera Drishti                         1    21.0     21.0       NA    21     21
-Total   OTHER   N4.40.8   Netra Roga - Drishtigata - Kaacha Roga                               2     6.5      6.5     7.78     1     12
M        OTHER   N4.40.8   Netra Roga - Drishtigata - Kaacha Roga                               2     6.5      6.5     7.78     1     12
-Total   OTHER   N4.40.9   Netra Roga - Drishtigata - Kaacha Roga - Kaphaja                     7     3.7      1.0     4.35     1     11
F        OTHER   N4.40.9   Netra Roga - Drishtigata - Kaacha Roga - Kaphaja                     3     1.3      1.0     0.58     1      2
M        OTHER   N4.40.9   Netra Roga - Drishtigata - Kaacha Roga - Kaphaja                     4     5.5      5.0     5.26     1     11
-Total   OTHER   N4.42.0   Netra Roga - Krishnagata                                             2    62.0     62.0    86.27     1    123
F        OTHER   N4.42.0   Netra Roga - Krishnagata                                             2    62.0     62.0    86.27     1    123
-Total   OTHER   N4.42.2.1   Netra Roga - Krishnagata - Shukra Netra Roga - Avrana Shukra       2     1.0      1.0     0.00     1      1
M        OTHER   N4.42.2.1   Netra Roga - Krishnagata - Shukra Netra Roga - Avrana Shukra       2     1.0      1.0     0.00     1      1
-Total   OTHER   N4.44   Netra Roga - Pittaja                                                   1     1.0      1.0       NA     1      1
F        OTHER   N4.44   Netra Roga - Pittaja                                                   1     1.0      1.0       NA     1      1
-Total   OTHER   N4.45   Netra Roga - Raktaja                                                   1    99.0     99.0       NA    99     99
M        OTHER   N4.45   Netra Roga - Raktaja                                                   1    99.0     99.0       NA    99     99
-Total   OTHER   N4.46.0   Netra Roga - Shuklagata                                              1     1.0      1.0       NA     1      1
M        OTHER   N4.46.0   Netra Roga - Shuklagata                                              1     1.0      1.0       NA     1      1
-Total   OTHER   N4.46.2.0   Netra Roga - Shuklagata - Arma                                     7     4.6      4.0     4.39     1     13
F        OTHER   N4.46.2.0   Netra Roga - Shuklagata - Arma                                     2     3.0      3.0     2.83     1      5
M        OTHER   N4.46.2.0   Netra Roga - Shuklagata - Arma                                     5     5.2      4.0     5.02     1     13
-Total   OTHER   N4.46.2.1   Netra Roga - Shuklagata - Arma - Adhimaamsa Arma                   3   246.3      1.0   424.93     1    737
F        OTHER   N4.46.2.1   Netra Roga - Shuklagata - Arma - Adhimaamsa Arma                   1     1.0      1.0       NA     1      1
M        OTHER   N4.46.2.1   Netra Roga - Shuklagata - Arma - Adhimaamsa Arma                   2   369.0    369.0   520.43     1    737
-Total   OTHER   N4.46.2.2   Netra Roga - Shuklagata - Arma - Lohitaarma                        1     1.0      1.0       NA     1      1
M        OTHER   N4.46.2.2   Netra Roga - Shuklagata - Arma - Lohitaarma                        1     1.0      1.0       NA     1      1
-Total   OTHER   N4.46.2.3   Netra Roga - Shuklagata - Arma - Prastaari Arma                    9    35.7      1.0    70.10     1    184
F        OTHER   N4.46.2.3   Netra Roga - Shuklagata - Arma - Prastaari Arma                    5    63.4      1.0    87.55     1    184
M        OTHER   N4.46.2.3   Netra Roga - Shuklagata - Arma - Prastaari Arma                    4     1.0      1.0     0.00     1      1
-Total   OTHER   N4.46.2.4   Netra Roga - Shuklagata - Arma - Shuklaarma                        1     6.0      6.0       NA     6      6
F        OTHER   N4.46.2.4   Netra Roga - Shuklagata - Arma - Shuklaarma                        1     6.0      6.0       NA     6      6
-Total   OTHER   N4.46.2.5   Netra Roga - Shuklagata - Arma - Snaavyaarma                       1     1.0      1.0       NA     1      1
M        OTHER   N4.46.2.5   Netra Roga - Shuklagata - Arma - Snaavyaarma                       1     1.0      1.0       NA     1      1
-Total   OTHER   N4.47.6   Netra Roga - Shuklagata - Siraaharsha                                2     5.0      5.0     5.66     1      9
F        OTHER   N4.47.6   Netra Roga - Shuklagata - Siraaharsha                                1     9.0      9.0       NA     9      9
M        OTHER   N4.47.6   Netra Roga - Shuklagata - Siraaharsha                                1     1.0      1.0       NA     1      1
-Total   OTHER   N4.47.7   Netra Roga - Shuklagata - Siraaja Pidakaa                            1    64.0     64.0       NA    64     64
F        OTHER   N4.47.7   Netra Roga - Shuklagata - Siraaja Pidakaa                            1    64.0     64.0       NA    64     64
-Total   OTHER   N4.49.0   Netra Roga - Sandhigata                                              4     1.0      1.0     0.00     1      1
F        OTHER   N4.49.0   Netra Roga - Sandhigata                                              1     1.0      1.0       NA     1      1
M        OTHER   N4.49.0   Netra Roga - Sandhigata                                              3     1.0      1.0     0.00     1      1
-Total   OTHER   N4.49.2   Netra Roga - Sandhigata - Krimigranthi                               3     1.7      1.0     1.15     1      3
F        OTHER   N4.49.2   Netra Roga - Sandhigata - Krimigranthi                               2     1.0      1.0     0.00     1      1
M        OTHER   N4.49.2   Netra Roga - Sandhigata - Krimigranthi                               1     3.0      3.0       NA     3      3
-Total   OTHER   N4.49.5   Netra Roga - Sandhigata - Pooyaalasa                                 1     1.0      1.0       NA     1      1
M        OTHER   N4.49.5   Netra Roga - Sandhigata - Pooyaalasa                                 1     1.0      1.0       NA     1      1
-Total   OTHER   N4.50.0   Netra Roga - Sarvagata                                               1    21.0     21.0       NA    21     21
F        OTHER   N4.50.0   Netra Roga - Sarvagata                                               1    21.0     21.0       NA    21     21
-Total   OTHER   N4.50.1.0   Netra Roga - Sarvagata - Abhishyanda                               1     1.0      1.0       NA     1      1
M        OTHER   N4.50.1.0   Netra Roga - Sarvagata - Abhishyanda                               1     1.0      1.0       NA     1      1
-Total   OTHER   N4.50.1.5   Netra Roga - Sarvagata - Abhishyanda - Vaataja Abhishyanda         5     9.2      1.0    18.34     1     42
F        OTHER   N4.50.1.5   Netra Roga - Sarvagata - Abhishyanda - Vaataja Abhishyanda         2     1.0      1.0     0.00     1      1
M        OTHER   N4.50.1.5   Netra Roga - Sarvagata - Abhishyanda - Vaataja Abhishyanda         3    14.7      1.0    23.67     1     42
-Total   OTHER   N4.50.10   Netra Roga - Sarvagata - Sashopha Akshipaaka                        1     1.0      1.0       NA     1      1
M        OTHER   N4.50.10   Netra Roga - Sarvagata - Sashopha Akshipaaka                        1     1.0      1.0       NA     1      1
-Total   OTHER   N4.50.2.0   Netra Roga - Sarvagata - Adhimantha                                8    94.8     38.5   109.25     1    238
F        OTHER   N4.50.2.0   Netra Roga - Sarvagata - Adhimantha                                3    70.0      6.0   115.21     1    203
M        OTHER   N4.50.2.0   Netra Roga - Sarvagata - Adhimantha                                5   109.6     57.0   116.25     1    238
-Total   OTHER   N4.50.2.1   Netra Roga - Sarvagata - Adhimantha - Kaphaja Adhimantha           6    74.8      2.5   171.66     1    425
F        OTHER   N4.50.2.1   Netra Roga - Sarvagata - Adhimantha - Kaphaja Adhimantha           5     4.8      1.0     6.94     1     17
M        OTHER   N4.50.2.1   Netra Roga - Sarvagata - Adhimantha - Kaphaja Adhimantha           1   425.0    425.0       NA   425    425
-Total   OTHER   N4.50.2.4   Netra Roga - Sarvagata - Adhimantha - Vaataja Adhimantha           2    30.0     30.0    41.01     1     59
M        OTHER   N4.50.2.4   Netra Roga - Sarvagata - Adhimantha - Vaataja Adhimantha           2    30.0     30.0    41.01     1     59
-Total   OTHER   N4.50.8   Netra Roga - Sarvagata - Hataadhimantha                              1     1.0      1.0       NA     1      1
M        OTHER   N4.50.8   Netra Roga - Sarvagata - Hataadhimantha                              1     1.0      1.0       NA     1      1
-Total   OTHER   N4.50.9   Netra Roga - Sarvagata - Shushkaakshi Paaka                         21     8.0      1.0    15.72     1     55
F        OTHER   N4.50.9   Netra Roga - Sarvagata - Shushkaakshi Paaka                         15     5.6      1.0    13.76     1     55
M        OTHER   N4.50.9   Netra Roga - Sarvagata - Shushkaakshi Paaka                          6    13.8      1.0    20.00     1     43
         OTHER   N4.51.0   Netra Roga - Vaataja                                                 1    25.0     25.0       NA    25     25
-Total   OTHER   N4.51.0   Netra Roga - Vaataja                                                 2    13.0     13.0    16.97     1     25
M        OTHER   N4.51.0   Netra Roga - Vaataja                                                 1     1.0      1.0       NA     1      1
-Total   OTHER   N4.52.3   Netra Roga - Vartmagata - Anjananaamikaa                             1     1.0      1.0       NA     1      1
F        OTHER   N4.52.3   Netra Roga - Vartmagata - Anjananaamikaa                             1     1.0      1.0       NA     1      1
-Total   OTHER   N4.53.11   Netra Roga - Vartmagata - Krichchhronmeelana                        2     1.0      1.0     0.00     1      1
F        OTHER   N4.53.11   Netra Roga - Vartmagata - Krichchhronmeelana                        1     1.0      1.0       NA     1      1
M        OTHER   N4.53.11   Netra Roga - Vartmagata - Krichchhronmeelana                        1     1.0      1.0       NA     1      1
-Total   OTHER   N4.53.13.0   Netra Roga - Vartmagata - Kumbheekaa                              1     1.0      1.0       NA     1      1
M        OTHER   N4.53.13.0   Netra Roga - Vartmagata - Kumbheekaa                              1     1.0      1.0       NA     1      1
-Total   OTHER   N4.53.13.1   Netra Roga - Vartmagata - Kumbheekaa - Atilikhita Vartma          1     1.0      1.0       NA     1      1
M        OTHER   N4.53.13.1   Netra Roga - Vartmagata - Kumbheekaa - Atilikhita Vartma          1     1.0      1.0       NA     1      1
-Total   OTHER   N4.54.14   Netra Roga - Vartmagata - Lagana                                    1     1.0      1.0       NA     1      1
M        OTHER   N4.54.14   Netra Roga - Vartmagata - Lagana                                    1     1.0      1.0       NA     1      1
-Total   OTHER   N4.54.15   Netra Roga - Vartmagata - Nimesha                                   1   609.0    609.0       NA   609    609
M        OTHER   N4.54.15   Netra Roga - Vartmagata - Nimesha                                   1   609.0    609.0       NA   609    609
-Total   OTHER   N4.54.26   Netra Roga - Vartmagata - Shushkaarsha                              1    17.0     17.0       NA    17     17
M        OTHER   N4.54.26   Netra Roga - Vartmagata - Shushkaarsha                              1    17.0     17.0       NA    17     17
-Total   OTHER   N4.55.30   Netra Roga - Vartmagata - Utsanga                                   2     1.0      1.0     0.00     1      1
F        OTHER   N4.55.30   Netra Roga - Vartmagata - Utsanga                                   1     1.0      1.0       NA     1      1
M        OTHER   N4.55.30   Netra Roga - Vartmagata - Utsanga                                   1     1.0      1.0       NA     1      1
-Total   OTHER   N4.55.31   Netra Roga - Vartmagata - Vaatahata                                 5     1.4      1.0     0.89     1      3
F        OTHER   N4.55.31   Netra Roga - Vartmagata - Vaatahata                                 2     1.0      1.0     0.00     1      1
M        OTHER   N4.55.31   Netra Roga - Vartmagata - Vaatahata                                 3     1.7      1.0     1.15     1      3
-Total   OTHER   N4.6   Netra Roga - Ashrusraava                                                5     7.6      1.0    14.76     1     34
F        OTHER   N4.6   Netra Roga - Ashrusraava                                                3    12.0      1.0    19.05     1     34
M        OTHER   N4.6   Netra Roga - Ashrusraava                                                2     1.0      1.0     0.00     1      1
-Total   OTHER   N5.0   Nidra Viparyaya / Nidra Bhanga                                          6     9.5      3.0    16.56     1     43
F        OTHER   N5.0   Nidra Viparyaya / Nidra Bhanga                                          2    24.0     24.0    26.87     5     43
M        OTHER   N5.0   Nidra Viparyaya / Nidra Bhanga                                          4     2.2      1.0     2.50     1      6
-Total   OTHER   O1.0   Ojavikriti                                                              4   504.0    347.5   634.55     1   1320
M        OTHER   O1.0   Ojavikriti                                                              4   504.0    347.5   634.55     1   1320
-Total   OTHER   O1.2   Ojavikriti - Ojakshaya                                                  5    52.8      1.0   110.32     1    250
F        OTHER   O1.2   Ojavikriti - Ojakshaya                                                  3    84.0      1.0   143.76     1    250
M        OTHER   O1.2   Ojavikriti - Ojakshaya                                                  2     6.0      6.0     7.07     1     11
-Total   OTHER   O1.3   Ojavikriti - Ojaksheenataa                                              1     1.0      1.0       NA     1      1
F        OTHER   O1.3   Ojavikriti - Ojaksheenataa                                              1     1.0      1.0       NA     1      1
-Total   OTHER   O1.4   Ojavikriti - Ojovisramsa                                               27    78.8     24.0   140.68     1    580
F        OTHER   O1.4   Ojavikriti - Ojovisramsa                                               13    93.7      1.0   190.76     1    580
M        OTHER   O1.4   Ojavikriti - Ojovisramsa                                               14    65.0     29.0    74.59     1    211
-Total   OTHER   P2.0   Pandu                                                                  54    51.7      1.0   124.30     1    701
F        OTHER   P2.0   Pandu                                                                  49    55.4      1.0   129.72     1    701
M        OTHER   P2.0   Pandu                                                                   5    14.6      1.0    30.41     1     69
-Total   OTHER   P2.2   Pandu- pitta                                                            4     5.5      1.0     9.00     1     19
F        OTHER   P2.2   Pandu- pitta                                                            4     5.5      1.0     9.00     1     19
-Total   OTHER   P3.0   Pidaka                                                                 16    33.9      1.0    93.32     1    375
F        OTHER   P3.0   Pidaka                                                                  7     2.9      1.0     3.48     1     10
M        OTHER   P3.0   Pidaka                                                                  9    58.1      1.0   121.72     1    375
-Total   OTHER   P5.5   Pramehajanya - Netraroga                                               25    70.9      2.0   129.35     1    513
F        OTHER   P5.5   Pramehajanya - Netraroga                                               12    39.3      1.0    78.01     1    262
M        OTHER   P5.5   Pramehajanya - Netraroga                                               13   100.0     18.0   161.15     1    513
-Total   OTHER   P6.0   Pravaahika                                                              1    39.0     39.0       NA    39     39
M        OTHER   P6.0   Pravaahika                                                              1    39.0     39.0       NA    39     39
-Total   OTHER   R2.0   Raktapitta                                                              8    21.5      1.0    47.91     1    139
F        OTHER   R2.0   Raktapitta                                                              5     4.6      1.0     8.05     1     19
M        OTHER   R2.0   Raktapitta                                                              3    49.7      9.0    77.47     1    139
-Total   OTHER   S0.0   Saamavata                                                              67    98.7     14.0   208.50     1   1120
F        OTHER   S0.0   Saamavata                                                              44    87.9     14.5   168.26     1    663
M        OTHER   S0.0   Saamavata                                                              23   119.3      7.0   272.76     1   1120
-Total   OTHER   S1.0   Sheetapitta                                                            51   112.5      1.0   276.37     1   1411
F        OTHER   S1.0   Sheetapitta                                                            35   117.8      1.0   287.89     1   1411
M        OTHER   S1.0   Sheetapitta                                                            16   100.9      1.0   257.95     1    991
-Total   OTHER   S11.0   stanya roga                                                            1     1.0      1.0       NA     1      1
F        OTHER   S11.0   stanya roga                                                            1     1.0      1.0       NA     1      1
-Total   OTHER   S12.0   Stanya dushti Sootika Roga                                             1     1.0      1.0       NA     1      1
F        OTHER   S12.0   Stanya dushti Sootika Roga                                             1     1.0      1.0       NA     1      1
-Total   OTHER   S14.1   Sthaanabhedena Shoola - Aamaashaya Shoola                             28    79.0      1.0   236.91     1   1174
F        OTHER   S14.1   Sthaanabhedena Shoola - Aamaashaya Shoola                             16    54.1      1.0   129.69     1    389
M        OTHER   S14.1   Sthaanabhedena Shoola - Aamaashaya Shoola                             12   112.2      1.0   335.74     1   1174
-Total   OTHER   S14.2   Sthaanabhedena Shoola - Aantra Shoola                                 12    19.3      3.5    31.04     1     96
F        OTHER   S14.2   Sthaanabhedena Shoola - Aantra Shoola                                  6    20.8      4.5    37.42     1     96
M        OTHER   S14.2   Sthaanabhedena Shoola - Aantra Shoola                                  6    17.8      3.5    26.72     1     67
-Total   OTHER   S14.22   Sthaanabhedena Shoola - Koshtha Shoola                                1    24.0     24.0       NA    24     24
M        OTHER   S14.22   Sthaanabhedena Shoola - Koshtha Shoola                                1    24.0     24.0       NA    24     24
-Total   OTHER   S14.25.0   Sthaanabhedena Shoola - Medhra Shoola                               1     1.0      1.0       NA     1      1
F        OTHER   S14.25.0   Sthaanabhedena Shoola - Medhra Shoola                               1     1.0      1.0       NA     1      1
-Total   OTHER   S15.30   Sthaanabhedena Shoola - Ooru Shoola                                   1     1.0      1.0       NA     1      1
F        OTHER   S15.30   Sthaanabhedena Shoola - Ooru Shoola                                   1     1.0      1.0       NA     1      1
-Total   OTHER   S15.33   Sthaanabhedena Shoola - Pakvaashaya Shoola                            2     1.0      1.0     0.00     1      1
F        OTHER   S15.33   Sthaanabhedena Shoola - Pakvaashaya Shoola                            1     1.0      1.0       NA     1      1
M        OTHER   S15.33   Sthaanabhedena Shoola - Pakvaashaya Shoola                            1     1.0      1.0       NA     1      1
-Total   OTHER   S15.38   Sthaanabhedena Shoola - Shirah Shoola                                36    77.4      1.0   210.26     1   1148
F        OTHER   S15.38   Sthaanabhedena Shoola - Shirah Shoola                                33    83.6      1.0   218.79     1   1148
M        OTHER   S15.38   Sthaanabhedena Shoola - Shirah Shoola                                 3     9.3      1.0    14.43     1     26
-Total   OTHER   S15.50.0   Sthaanabhedena Shoola - Vrishana Shoola                             1   150.0    150.0       NA   150    150
M        OTHER   S15.50.0   Sthaanabhedena Shoola - Vrishana Shoola                             1   150.0    150.0       NA   150    150
-Total   OTHER   S16.1   Stree Roga                                                            76   105.0      2.5   227.98     1   1291
F        OTHER   S16.1   Stree Roga                                                            75   106.4      3.0   229.19     1   1291
M        OTHER   S16.1   Stree Roga                                                             1     1.0      1.0       NA     1      1
-Total   OTHER   S16.10   Stree Roga - Shvetapradara                                           36    41.6      1.0   125.42     1    564
F        OTHER   S16.10   Stree Roga - Shvetapradara                                           36    41.6      1.0   125.42     1    564
-Total   OTHER   S16.12   Stree Roga - Yonigata Sraava                                          7    12.0      1.0    26.55     1     72
F        OTHER   S16.12   Stree Roga - Yonigata Sraava                                          7    12.0      1.0    26.55     1     72
-Total   OTHER   S16.19.0   Stree Roga - Rajo Dushti                                            5    11.2      9.0    12.05     1     30
F        OTHER   S16.19.0   Stree Roga - Rajo Dushti                                            5    11.2      9.0    12.05     1     30
-Total   OTHER   S16.2   Stree Roga - Aartavanaasha                                             2    16.5     16.5    13.44     7     26
F        OTHER   S16.2   Stree Roga - Aartavanaasha                                             2    16.5     16.5    13.44     7     26
-Total   OTHER   S16.20.0   Stree Roga - Raktapradara                                          36   118.9      2.5   299.92     1   1239
F        OTHER   S16.20.0   Stree Roga - Raktapradara                                          36   118.9      2.5   299.92     1   1239
-Total   OTHER   S16.20.7   STREEROGA - RAJONIVRITHI                                            3     1.3      1.0     0.58     1      2
F        OTHER   S16.20.7   STREEROGA - RAJONIVRITHI                                            3     1.3      1.0     0.58     1      2
-Total   OTHER   S16.20.8   STREEROGA - RAJONIVRITHI VIKARA                                    13    56.8     10.0   103.42     1    368
F        OTHER   S16.20.8   STREEROGA - RAJONIVRITHI VIKARA                                    13    56.8     10.0   103.42     1    368
-Total   OTHER   S16.21.0   Stree Roga - Sootikaa Roga                                          6    15.5      9.5    21.64     1     58
F        OTHER   S16.21.0   Stree Roga - Sootikaa Roga                                          6    15.5      9.5    21.64     1     58
-Total   OTHER   S16.21.2   Stree Roga - Sootikaa Roga - Koshtha Shodhanaartha                  1    22.0     22.0       NA    22     22
F        OTHER   S16.21.2   Stree Roga - Sootikaa Roga - Koshtha Shodhanaartha                  1    22.0     22.0       NA    22     22
-Total   OTHER   S16.21.4   STREEROGA - SOOTIKAA PARICHARYA                                     3    76.7     19.0   112.22     5    206
F        OTHER   S16.21.4   STREEROGA - SOOTIKAA PARICHARYA                                     3    76.7     19.0   112.22     5    206
-Total   OTHER   S16.22   Stree Roga - Stana Roga                                              11     7.7      1.0    14.19     1     44
F        OTHER   S16.22   Stree Roga - Stana Roga                                              11     7.7      1.0    14.19     1     44
-Total   OTHER   S16.27   Stree Roga - Stana Roga - Vaataja                                     1     4.0      4.0       NA     4      4
F        OTHER   S16.27   Stree Roga - Stana Roga - Vaataja                                     1     4.0      4.0       NA     4      4
-Total   OTHER   S16.29.0   Stree Roga - Stanyanaasha                                           1     1.0      1.0       NA     1      1
F        OTHER   S16.29.0   Stree Roga - Stanyanaasha                                           1     1.0      1.0       NA     1      1
-Total   OTHER   S16.29.1   Stree Roga - Yoni Kandoo                                           11    13.5      7.0    21.37     1     72
F        OTHER   S16.29.1   Stree Roga - Yoni Kandoo                                           11    13.5      7.0    21.37     1     72
-Total   OTHER   S16.3   Stree Roga - Anaartava                                                 5     7.8      1.0    15.21     1     35
F        OTHER   S16.3   Stree Roga - Anaartava                                                 5     7.8      1.0    15.21     1     35
-Total   OTHER   S16.30.0   Stree Roga - Yonivyaapad                                            8    39.8      1.0   106.41     1    303
F        OTHER   S16.30.0   Stree Roga - Yonivyaapad                                            8    39.8      1.0   106.41     1    303
-Total   OTHER   S16.30.10   Stree Roga - Yonivyaapad - Karninee                               12     7.0      1.0    17.87     1     63
F        OTHER   S16.30.10   Stree Roga - Yonivyaapad - Karninee                               12     7.0      1.0    17.87     1     63
-Total   OTHER   S16.30.12   Stree Roga - Yonivyaapad - Mahaayoni                               6     9.7      8.5     9.29     1     26
F        OTHER   S16.30.12   Stree Roga - Yonivyaapad - Mahaayoni                               6     9.7      8.5     9.29     1     26
-Total   OTHER   S16.30.13   Stree Roga - Yonivyaapad - Pariplutaa                             14    37.5      1.5    80.27     1    306
F        OTHER   S16.30.13   Stree Roga - Yonivyaapad - Pariplutaa                             14    37.5      1.5    80.27     1    306
-Total   OTHER   S16.30.14   Stree Roga - Yonivyaapad - Phalinee / Andalee                      1     1.0      1.0       NA     1      1
F        OTHER   S16.30.14   Stree Roga - Yonivyaapad - Phalinee / Andalee                      1     1.0      1.0       NA     1      1
-Total   OTHER   S16.30.15   Stree Roga - Yonivyaapad - Pittalaa                                5     7.6      2.0    12.58     1     30
F        OTHER   S16.30.15   Stree Roga - Yonivyaapad - Pittalaa                                5     7.6      2.0    12.58     1     30
-Total   OTHER   S16.30.22   Stree Roga - Yonivyaapad - Sramsinee                               4     2.5      1.0     3.00     1      7
F        OTHER   S16.30.22   Stree Roga - Yonivyaapad - Sramsinee                               4     2.5      1.0     3.00     1      7
-Total   OTHER   S16.30.23   Stree Roga - Yonivyaapad - Udaavartaa                              5    17.0      1.0    35.78     1     81
F        OTHER   S16.30.23   Stree Roga - Yonivyaapad - Udaavartaa                              5    17.0      1.0    35.78     1     81
-Total   OTHER   S16.30.26   Stree Roga - Yonivyaapad - Vaatalaa                                6     2.0      1.0     2.45     1      7
F        OTHER   S16.30.26   Stree Roga - Yonivyaapad - Vaatalaa                                6     2.0      1.0     2.45     1      7
-Total   OTHER   S16.30.27   Stree Roga - Yonivyaapad - Vandhyaa                                1     3.0      3.0       NA     3      3
F        OTHER   S16.30.27   Stree Roga - Yonivyaapad - Vandhyaa                                1     3.0      3.0       NA     3      3
-Total   OTHER   S16.30.9   Stree Roga - Yonivyaapad - Kaphajaa                                11     8.3      1.0    17.24     1     59
F        OTHER   S16.30.9   Stree Roga - Yonivyaapad - Kaphajaa                                11     8.3      1.0    17.24     1     59
-Total   OTHER   S16.7   Stree Roga - Krichchhraartava                                          2     1.0      1.0     0.00     1      1
F        OTHER   S16.7   Stree Roga - Krichchhraartava                                          2     1.0      1.0     0.00     1      1
-Total   OTHER   S16.9   Stree Roga - Raja Kshaya                                               6    28.8      1.0    54.81     1    138
F        OTHER   S16.9   Stree Roga - Raja Kshaya                                               6    28.8      1.0    54.81     1    138
-Total   OTHER   S17.0   Supti                                                                 64    62.3      1.0   138.82     1    722
F        OTHER   S17.0   Supti                                                                 28    68.0      9.5   147.19     1    602
M        OTHER   S17.0   Supti                                                                 36    57.8      1.0   133.89     1    722
-Total   OTHER   S17.1   Supti - Paada Supti                                                   19    33.6      1.0   101.64     1    444
F        OTHER   S17.1   Supti - Paada Supti                                                    8    63.6      1.0   154.25     1    444
M        OTHER   S17.1   Supti - Paada Supti                                                   11    11.7      1.0    26.32     1     89
-Total   OTHER   S18.0   Swastha                                                              144    50.8      3.0   173.15     1   1097
F        OTHER   S18.0   Swastha                                                               73    60.2      3.0   191.78     1   1097
M        OTHER   S18.0   Swastha                                                               71    41.2      1.0   152.43     1   1062
-Total   OTHER   S18.1   Swaasthya Pareeksha                                                   66    20.1      1.0   126.54     1   1017
F        OTHER   S18.1   Swaasthya Pareeksha                                                   35     2.0      1.0     2.75     1     13
M        OTHER   S18.1   Swaasthya Pareeksha                                                   31    40.6      1.0   184.03     1   1017
-Total   OTHER   S1A.2   Shoola - Annadrava Shoola                                              4     1.0      1.0     0.00     1      1
F        OTHER   S1A.2   Shoola - Annadrava Shoola                                              1     1.0      1.0       NA     1      1
M        OTHER   S1A.2   Shoola - Annadrava Shoola                                              3     1.0      1.0     0.00     1      1
-Total   OTHER   S1A.4   Shoola - Parinaama Shoola                                              5    11.0      9.0    11.98     1     30
F        OTHER   S1A.4   Shoola - Parinaama Shoola                                              4     6.2      5.0     6.40     1     14
M        OTHER   S1A.4   Shoola - Parinaama Shoola                                              1    30.0     30.0       NA    30     30
-Total   OTHER   S2.0   Shiroroga                                                              13     8.2      1.0    18.44     1     62
F        OTHER   S2.0   Shiroroga                                                              12     8.8      1.0    19.13     1     62
M        OTHER   S2.0   Shiroroga                                                               1     1.0      1.0       NA     1      1
-Total   OTHER   S2.10   Shiroroga - Shirahshoola                                             166    42.2      1.0   132.07     1   1074
F        OTHER   S2.10   Shiroroga - Shirahshoola                                             117    48.1      1.0   139.17     1   1074
M        OTHER   S2.10   Shiroroga - Shirahshoola                                              49    28.1      1.0   113.44     1    778
-Total   OTHER   S2.11   Shiroroga - Shirogaurava                                               2     1.0      1.0     0.00     1      1
F        OTHER   S2.11   Shiroroga - Shirogaurava                                               1     1.0      1.0       NA     1      1
M        OTHER   S2.11   Shiroroga - Shirogaurava                                               1     1.0      1.0       NA     1      1
-Total   OTHER   S2.13   Shiroroga - Anantavaata                                                1   609.0    609.0       NA   609    609
F        OTHER   S2.13   Shiroroga - Anantavaata                                                1   609.0    609.0       NA   609    609
-Total   OTHER   S2.14   Shiroroga - Ardhaavabhedaka                                           32    75.3      1.5   153.90     1    580
F        OTHER   S2.14   Shiroroga - Ardhaavabhedaka                                           26    91.1      1.5   167.20     1    580
M        OTHER   S2.14   Shiroroga - Ardhaavabhedaka                                            6     6.8      2.0    12.37     1     32
-Total   OTHER   S2.15.0   Shiroroga - Kapaalagata Shiroroga                                    1     1.0      1.0       NA     1      1
F        OTHER   S2.15.0   Shiroroga - Kapaalagata Shiroroga                                    1     1.0      1.0       NA     1      1
-Total   OTHER   S2.15.2   Shiroroga - Kapaalagata Shiroroga - Daarunaka                       19    62.3      1.0   165.07     1    668
F        OTHER   S2.15.2   Shiroroga - Kapaalagata Shiroroga - Daarunaka                       10    70.5      1.0   210.12     1    668
M        OTHER   S2.15.2   Shiroroga - Kapaalagata Shiroroga - Daarunaka                        9    53.1      1.0   107.05     1    323
-Total   OTHER   S2.15.4   Shiroroga - Kapaalagata Shiroroga - Khaalitya                        1     1.0      1.0       NA     1      1
M        OTHER   S2.15.4   Shiroroga - Kapaalagata Shiroroga - Khaalitya                        1     1.0      1.0       NA     1      1
-Total   OTHER   S2.15.5   Shirorog - Kapaalagata Shiroroga - Paalitya                          6    28.5      1.0    52.50     1    132
F        OTHER   S2.15.5   Shirorog - Kapaalagata Shiroroga - Paalitya                          3    44.7      1.0    75.63     1    132
M        OTHER   S2.15.5   Shirorog - Kapaalagata Shiroroga - Paalitya                          3    12.3      1.0    19.63     1     35
-Total   OTHER   S2.16.1   Shiroroga - Kaphaja Shiroroga - Shirahshoola                        12     3.8      1.0     6.28     1     21
F        OTHER   S2.16.1   Shiroroga - Kaphaja Shiroroga - Shirahshoola                         9     4.7      1.0     7.11     1     21
M        OTHER   S2.16.1   Shiroroga - Kaphaja Shiroroga - Shirahshoola                         3     1.0      1.0     0.00     1      1
-Total   OTHER   S2.19.1   Shiroroga - Pittaja Shiroroga - Shirahshoola                        10    58.5      2.0   127.27     1    404
F        OTHER   S2.19.1   Shiroroga - Pittaja Shiroroga - Shirahshoola                         8    72.9     12.5   140.16     1    404
M        OTHER   S2.19.1   Shiroroga - Pittaja Shiroroga - Shirahshoola                         2     1.0      1.0     0.00     1      1
-Total   OTHER   S2.23   Shiroroga - Shirah Kampa                                               3     1.7      1.0     1.15     1      3
F        OTHER   S2.23   Shiroroga - Shirah Kampa                                               3     1.7      1.0     1.15     1      3
-Total   OTHER   S2.24   Shiroroga - Sooryaavarta Shiroroga                                     1     1.0      1.0       NA     1      1
F        OTHER   S2.24   Shiroroga - Sooryaavarta Shiroroga                                     1     1.0      1.0       NA     1      1
-Total   OTHER   S2.25.2   Shiroroga - Vaataja Shiroroga - Shirahshoola                         4    15.0      1.0    28.00     1     57
F        OTHER   S2.25.2   Shiroroga - Vaataja Shiroroga - Shirahshoola                         3     1.0      1.0     0.00     1      1
M        OTHER   S2.25.2   Shiroroga - Vaataja Shiroroga - Shirahshoola                         1    57.0     57.0       NA    57     57
-Total   OTHER   S2.3   Shiroroga – Sahaja                                                    1     1.0      1.0       NA     1      1
F        OTHER   S2.3   Shiroroga – Sahaja                                                    1     1.0      1.0       NA     1      1
-Total   OTHER   S2.4   Shiroroga - Shankha Shoola                                              4    12.2      3.0    19.92     1     42
F        OTHER   S2.4   Shiroroga - Shankha Shoola                                              2    23.5     23.5    26.16     5     42
M        OTHER   S2.4   Shiroroga - Shankha Shoola                                              2     1.0      1.0     0.00     1      1
-Total   OTHER   S2.5   Shiroroga - Shira Abhihata                                              4     5.5      1.0     9.00     1     19
F        OTHER   S2.5   Shiroroga - Shira Abhihata                                              2     1.0      1.0     0.00     1      1
M        OTHER   S2.5   Shiroroga - Shira Abhihata                                              2    10.0     10.0    12.73     1     19
-Total   OTHER   S2.7   Shiroroga - Shira Supti                                                 2     1.0      1.0     0.00     1      1
F        OTHER   S2.7   Shiroroga - Shira Supti                                                 2     1.0      1.0     0.00     1      1
-Total   OTHER   S2.9   Shiroroga - Shirah Pradeshastha Paaka                                   1     1.0      1.0       NA     1      1
F        OTHER   S2.9   Shiroroga - Shirah Pradeshastha Paaka                                   1     1.0      1.0       NA     1      1
-Total   OTHER   S3.0   Shotha                                                                127    48.5      1.0   129.63     1   1059
F        OTHER   S3.0   Shotha                                                                 71    33.0      1.0    70.06     1    370
M        OTHER   S3.0   Shotha                                                                 56    68.1      1.0   177.60     1   1059
-Total   OTHER   S3.1   Shotha - Vaatapittaja                                                   2     2.0      2.0     1.41     1      3
F        OTHER   S3.1   Shotha - Vaatapittaja                                                   1     1.0      1.0       NA     1      1
M        OTHER   S3.1   Shotha - Vaatapittaja                                                   1     3.0      3.0       NA     3      3
-Total   OTHER   S3.3   Shrama                                                                 64    86.5      1.0   243.02     1   1219
F        OTHER   S3.3   Shrama                                                                 41    72.2      1.0   222.46     1   1219
M        OTHER   S3.3   Shrama                                                                 23   112.1      1.0   279.42     1   1152
-Total   OTHER   S3A.0   Shrama                                                                 2    27.5     27.5    17.68    15     40
M        OTHER   S3A.0   Shrama                                                                 2    27.5     27.5    17.68    15     40
-Total   OTHER   S4.0   Shvaasa                                                               201   119.1      8.0   268.10     1   1884
F        OTHER   S4.0   Shvaasa                                                               121   133.3      6.0   312.13     1   1884
M        OTHER   S4.0   Shvaasa                                                                80    97.7     15.5   182.26     1    803
-Total   OTHER   S4.2   Shvaasa-kshudra shvasa                                                  2     1.0      1.0     0.00     1      1
F        OTHER   S4.2   Shvaasa-kshudra shvasa                                                  1     1.0      1.0       NA     1      1
M        OTHER   S4.2   Shvaasa-kshudra shvasa                                                  1     1.0      1.0       NA     1      1
-Total   OTHER   S4.4   Shvaasa-tamaka shvasa                                                  16    33.4      1.0    85.77     1    340
F        OTHER   S4.4   Shvaasa-tamaka shvasa                                                  10     7.7      1.0    14.68     1     43
M        OTHER   S4.4   Shvaasa-tamaka shvasa                                                   6    76.2      8.0   134.77     1    340
-Total   OTHER   S5.0   Shvitra                                                                20   159.7      3.5   259.47     1    918
F        OTHER   S5.0   Shvitra                                                                 8   162.0     69.5   241.61     1    715
M        OTHER   S5.0   Shvitra                                                                12   158.2      1.0   281.31     1    918
-Total   OTHER   S8.0   Snaayuka                                                                1     1.0      1.0       NA     1      1
F        OTHER   S8.0   Snaayuka                                                                1     1.0      1.0       NA     1      1
-Total   OTHER   S9.0   Somaroga                                                                1     1.0      1.0       NA     1      1
M        OTHER   S9.0   Somaroga                                                                1     1.0      1.0       NA     1      1
-Total   OTHER   U1.0   Udaavarta                                                              25    24.7      1.0    49.55     1    197
F        OTHER   U1.0   Udaavarta                                                              17    24.3      1.0    39.77     1    116
M        OTHER   U1.0   Udaavarta                                                               8    25.5      1.0    69.30     1    197
-Total   OTHER   U2.0   Udara                                                                   7    31.0      3.0    48.95     1    116
F        OTHER   U2.0   Udara                                                                   2     5.5      5.5     3.54     3      8
M        OTHER   U2.0   Udara                                                                   5    41.2      1.0    55.99     1    116
-Total   OTHER   U2.3   Udara- vata                                                             2     5.0      5.0     5.66     1      9
F        OTHER   U2.3   Udara- vata                                                             1     1.0      1.0       NA     1      1
M        OTHER   U2.3   Udara- vata                                                             1     9.0      9.0       NA     9      9
-Total   OTHER   U2.4   Udara- yakrut-pleeha                                                   16   119.9      1.0   261.79     1   1018
F        OTHER   U2.4   Udara- yakrut-pleeha                                                    4    78.5     36.0   113.25     1    241
M        OTHER   U2.4   Udara- yakrut-pleeha                                                   12   133.8      1.0   298.54     1   1018
-Total   OTHER   U2.6   Udara-jalodara                                                          3   316.0    346.0   301.12     1    601
F        OTHER   U2.6   Udara-jalodara                                                          1     1.0      1.0       NA     1      1
M        OTHER   U2.6   Udara-jalodara                                                          2   473.5    473.5   180.31   346    601
-Total   OTHER   U2.8   Udara-sannipataja                                                       1     1.0      1.0       NA     1      1
M        OTHER   U2.8   Udara-sannipataja                                                       1     1.0      1.0       NA     1      1
-Total   OTHER   U3.0   Udarda                                                                 14    53.6      1.0   116.15     1    371
F        OTHER   U3.0   Udarda                                                                  9    42.1      1.0   123.33     1    371
M        OTHER   U3.0   Udarda                                                                  5    74.4      1.0   112.19     1    255
-Total   OTHER   U4.0   Unmaada                                                                14   107.1     27.0   171.72     1    536
F        OTHER   U4.0   Unmaada                                                                 6    58.2      4.0   109.45     1    276
M        OTHER   U4.0   Unmaada                                                                 8   143.8     35.5   206.44     1    536
-Total   OTHER   U4.5   Unmaada - Kaphaja                                                       1     1.0      1.0       NA     1      1
M        OTHER   U4.5   Unmaada - Kaphaja                                                       1     1.0      1.0       NA     1      1
-Total   OTHER   U4.7   Unmaada - Manokhedaja                                                   6    69.3     24.5    93.30     1    224
F        OTHER   U4.7   Unmaada - Manokhedaja                                                   4    68.5     24.5   106.01     1    224
M        OTHER   U4.7   Unmaada - Manokhedaja                                                   2    71.0     71.0    98.99     1    141
-Total   OTHER   U5.0   Urahkshata                                                              1     1.0      1.0       NA     1      1
F        OTHER   U5.0   Urahkshata                                                              1     1.0      1.0       NA     1      1
-Total   OTHER   V.10   Vatavyadhi - Avrita Vata                                                6    31.7     40.5    25.15     1     59
F        OTHER   V.10   Vatavyadhi - Avrita Vata                                                5    28.4     33.0    26.66     1     59
M        OTHER   V.10   Vatavyadhi - Avrita Vata                                                1    48.0     48.0       NA    48     48
-Total   OTHER   V10.11   Vatavyadhi - Avrita Vata - Mamsavrita Vata                            4    10.5      2.5    17.06     1     36
F        OTHER   V10.11   Vatavyadhi - Avrita Vata - Mamsavrita Vata                            2     2.5      2.5     2.12     1      4
M        OTHER   V10.11   Vatavyadhi - Avrita Vata - Mamsavrita Vata                            2    18.5     18.5    24.75     1     36
-Total   OTHER   V10.20   Vatavyadhi - Avrita Vata - Prana avrita Samana                        1    36.0     36.0       NA    36     36
F        OTHER   V10.20   Vatavyadhi - Avrita Vata - Prana avrita Samana                        1    36.0     36.0       NA    36     36
-Total   OTHER   V10.24   Vatavyadhi - Avrita Vata - Raktavrita Vata                            2    25.0     25.0    16.97    13     37
F        OTHER   V10.24   Vatavyadhi - Avrita Vata - Raktavrita Vata                            1    37.0     37.0       NA    37     37
M        OTHER   V10.24   Vatavyadhi - Avrita Vata - Raktavrita Vata                            1    13.0     13.0       NA    13     13
-Total   OTHER   V10.29   Vatavyadhi - Avrita Vata - Udana avrita Prana                         1     9.0      9.0       NA     9      9
F        OTHER   V10.29   Vatavyadhi - Avrita Vata - Udana avrita Prana                         1     9.0      9.0       NA     9      9
-Total   OTHER   V10.8   Vatavyadhi - Avrita Vata - Kapha avrita Vata                           1    39.0     39.0       NA    39     39
F        OTHER   V10.8   Vatavyadhi - Avrita Vata - Kapha avrita Vata                           1    39.0     39.0       NA    39     39
-Total   OTHER   V11.1   Vatavyadhi - Gatatva  - Amashayagata Vata                              1     1.0      1.0       NA     1      1
F        OTHER   V11.1   Vatavyadhi - Gatatva  - Amashayagata Vata                              1     1.0      1.0       NA     1      1
-Total   OTHER   V11.10   Vatavyadhi - Gatatva  - Siragata Vata                                 1    22.0     22.0       NA    22     22
F        OTHER   V11.10   Vatavyadhi - Gatatva  - Siragata Vata                                 1    22.0     22.0       NA    22     22
-Total   OTHER   V11.4   Vatavyadhi - Gatatva  - Mamsamedogata Vata                             1    22.0     22.0       NA    22     22
F        OTHER   V11.4   Vatavyadhi - Gatatva  - Mamsamedogata Vata                             1    22.0     22.0       NA    22     22
-Total   OTHER   V11.6   Vatavyadhi - Gatatva  - Raktagata Vata                                 2     3.5      3.5     3.54     1      6
F        OTHER   V11.6   Vatavyadhi - Gatatva  - Raktagata Vata                                 2     3.5      3.5     3.54     1      6
-Total   OTHER   V11.7   Vatavyadhi - Gatatva  - Sandhigata Vata                               13    20.7      7.0    38.23     1    125
F        OTHER   V11.7   Vatavyadhi - Gatatva  - Sandhigata Vata                                8    21.5      9.0    42.15     1    125
M        OTHER   V11.7   Vatavyadhi - Gatatva  - Sandhigata Vata                                5    19.4      5.0    35.65     1     83
-Total   OTHER   V11.8   Vatavyadhi - Gatatva  - Sarvangagata Vata                              3    38.0     21.0    47.82     1     92
F        OTHER   V11.8   Vatavyadhi - Gatatva  - Sarvangagata Vata                              2    46.5     46.5    64.35     1     92
M        OTHER   V11.8   Vatavyadhi - Gatatva  - Sarvangagata Vata                              1    21.0     21.0       NA    21     21
-Total   OTHER   V12.0   Visphota                                                               1     1.0      1.0       NA     1      1
M        OTHER   V12.0   Visphota                                                               1     1.0      1.0       NA     1      1
-Total   OTHER   V13.0   Vrana                                                                 38    54.8      3.5   155.48     1    839
F        OTHER   V13.0   Vrana                                                                 12    88.3      1.0   242.13     1    839
M        OTHER   V13.0   Vrana                                                                 26    39.4      7.5    95.88     1    433
-Total   OTHER   V13.1.0   Vrana - Aagantuja (Sadyo) Vrana                                      5     9.2      7.0     9.96     1     25
M        OTHER   V13.1.0   Vrana - Aagantuja (Sadyo) Vrana                                      5     9.2      7.0     9.96     1     25
-Total   OTHER   V13.1.2   Vrana - Aagantuja (Sadyo) Vrana - Chhinna Aagantuja Vrana            1     1.0      1.0       NA     1      1
M        OTHER   V13.1.2   Vrana - Aagantuja (Sadyo) Vrana - Chhinna Aagantuja Vrana            1     1.0      1.0       NA     1      1
-Total   OTHER   V13.1.3   Vrana - Aagantuja (Sadyo) Vrana - Dagdha Vrana                       3    38.0     14.0    53.23     1     99
F        OTHER   V13.1.3   Vrana - Aagantuja (Sadyo) Vrana - Dagdha Vrana                       1     1.0      1.0       NA     1      1
M        OTHER   V13.1.3   Vrana - Aagantuja (Sadyo) Vrana - Dagdha Vrana                       2    56.5     56.5    60.10    14     99
-Total   OTHER   V13.1.4   Vrana - Aagantuja (Sadyo) Vrana - Ghrishta Aagantuja Vrana           1     1.0      1.0       NA     1      1
F        OTHER   V13.1.4   Vrana - Aagantuja (Sadyo) Vrana - Ghrishta Aagantuja Vrana           1     1.0      1.0       NA     1      1
-Total   OTHER   V13.1.5   Vrana - Aagantuja (Sadyo) Vrana - Kshata Aagantuja Vrana             1     1.0      1.0       NA     1      1
M        OTHER   V13.1.5   Vrana - Aagantuja (Sadyo) Vrana - Kshata Aagantuja Vrana             1     1.0      1.0       NA     1      1
-Total   OTHER   V13.11   Vrana - Pitta Vaataja Vrana                                           1     1.0      1.0       NA     1      1
F        OTHER   V13.11   Vrana - Pitta Vaataja Vrana                                           1     1.0      1.0       NA     1      1
-Total   OTHER   V13.3   Vrana - Dushta Vrana                                                  34    71.9     18.5   144.92     1    750
F        OTHER   V13.3   Vrana - Dushta Vrana                                                  15    91.1     17.0   196.08     1    750
M        OTHER   V13.3   Vrana - Dushta Vrana                                                  19    56.8     20.0    89.74     1    378
-Total   OTHER   V13.4   Vrana - Jeerna Vrana                                                   1   135.0    135.0       NA   135    135
M        OTHER   V13.4   Vrana - Jeerna Vrana                                                   1   135.0    135.0       NA   135    135
-Total   OTHER   V13.8   Vrana - Kaphavaataraktaja Vrana                                        2     4.0      4.0     1.41     3      5
M        OTHER   V13.8   Vrana - Kaphavaataraktaja Vrana                                        2     4.0      4.0     1.41     3      5
-Total   OTHER   V15.0   Vriddhi                                                                4     3.0      1.0     4.00     1      9
F        OTHER   V15.0   Vriddhi                                                                1     1.0      1.0       NA     1      1
M        OTHER   V15.0   Vriddhi                                                                3     3.7      1.0     4.62     1      9
-Total   OTHER   V15.1   Vriddhi - Aantraja                                                     9   189.3      1.0   549.36     1   1654
F        OTHER   V15.1   Vriddhi - Aantraja                                                     4    11.5      5.0    16.11     1     35
M        OTHER   V15.1   Vriddhi - Aantraja                                                     5   331.6      1.0   739.24     1   1654
-Total   OTHER   V15.4   Vriddhi - Mootraja                                                     1     1.0      1.0       NA     1      1
M        OTHER   V15.4   Vriddhi - Mootraja                                                     1     1.0      1.0       NA     1      1
-Total   OTHER   V16.0   Vrikka Roga                                                           86   118.2     13.5   217.75     1    896
F        OTHER   V16.0   Vrikka Roga                                                           25   112.1     10.0   219.02     1    872
M        OTHER   V16.0   Vrikka Roga                                                           61   120.7     22.0   218.99     1    896
-Total   OTHER   V2.10   Vaatavyaadhi - Avabaahuka                                            967   123.7      8.0   263.32     1   1849
F        OTHER   V2.10   Vaatavyaadhi - Avabaahuka                                            603   126.5     12.0   260.01     1   1620
M        OTHER   V2.10   Vaatavyaadhi - Avabaahuka                                            364   119.1      1.0   269.01     1   1849
-Total   OTHER   V2.13   Vaatavyaadhi - Avasthaa - Supti                                        6    37.5      1.0    82.74     1    206
F        OTHER   V2.13   Vaatavyaadhi - Avasthaa - Supti                                        3     5.7      1.0     8.08     1     15
M        OTHER   V2.13   Vaatavyaadhi - Avasthaa - Supti                                        3    69.3      1.0   118.36     1    206
-Total   OTHER   V2.14   Vaatavyaadhi - Avasthaa - Toda                                         2   176.0    176.0   247.49     1    351
F        OTHER   V2.14   Vaatavyaadhi - Avasthaa - Toda                                         2   176.0    176.0   247.49     1    351
-Total   OTHER   V2.15   Vaatavyaadhi - Baadhirya                                               4    36.5     24.5    42.63     1     96
F        OTHER   V2.15   Vaatavyaadhi - Baadhirya                                               2    24.5     24.5    19.09    11     38
M        OTHER   V2.15   Vaatavyaadhi - Baadhirya                                               2    48.5     48.5    67.18     1     96
-Total   OTHER   V2.19   Vaatavyaadhi - Dandaapataanaka                                         1     7.0      7.0       NA     7      7
F        OTHER   V2.19   Vaatavyaadhi - Dandaapataanaka                                         1     7.0      7.0       NA     7      7
-Total   OTHER   V2.2   Vaatavyaadhi - Aadhmaana                                              154    61.9      1.0   166.44     1   1317
F        OTHER   V2.2   Vaatavyaadhi - Aadhmaana                                               61    41.8      1.0   111.37     1    589
M        OTHER   V2.2   Vaatavyaadhi - Aadhmaana                                               93    75.0      1.0   193.75     1   1317
-Total   OTHER   V2.21   Vaatavyaadhi - Ekaangavaata                                           13    10.7      1.0    26.61     1     97
F        OTHER   V2.21   Vaatavyaadhi - Ekaangavaata                                            7     5.3      1.0     7.87     1     21
M        OTHER   V2.21   Vaatavyaadhi - Ekaangavaata                                            6    17.0      1.0    39.19     1     97
-Total   OTHER   V2.24   Vaatavyaadhi - Gudasthita Vaata                                        2    32.5     32.5    44.55     1     64
F        OTHER   V2.24   Vaatavyaadhi - Gudasthita Vaata                                        2    32.5     32.5    44.55     1     64
-Total   OTHER   V2.25   Vaatavyaadhi - Hanugraha                                               2    48.5     48.5    67.18     1     96
F        OTHER   V2.25   Vaatavyaadhi - Hanugraha                                               2    48.5     48.5    67.18     1     96
-Total   OTHER   V2.28   Vaatavyaadhi - Hridayastha Vaata                                       1     1.0      1.0       NA     1      1
F        OTHER   V2.28   Vaatavyaadhi - Hridayastha Vaata                                       1     1.0      1.0       NA     1      1
-Total   OTHER   V2.3.0   Vaatavyaadhi - Aakshepaka                                             3    40.7      1.0    68.70     1    120
F        OTHER   V2.3.0   Vaatavyaadhi - Aakshepaka                                             1     1.0      1.0       NA     1      1
M        OTHER   V2.3.0   Vaatavyaadhi - Aakshepaka                                             2    60.5     60.5    84.15     1    120
-Total   OTHER   V2.3.1   Vaatavyaadhi - Aakshepaka - Abhighaataja                              3    23.0      1.0    38.11     1     67
F        OTHER   V2.3.1   Vaatavyaadhi - Aakshepaka - Abhighaataja                              1     1.0      1.0       NA     1      1
M        OTHER   V2.3.1   Vaatavyaadhi - Aakshepaka - Abhighaataja                              2    34.0     34.0    46.67     1     67
-Total   OTHER   V2.34.0   Vaatavyaadhi - Kampa                                               110   159.6     26.0   268.92     1   1427
F        OTHER   V2.34.0   Vaatavyaadhi - Kampa                                                33   211.9     26.0   326.73     1   1304
M        OTHER   V2.34.0   Vaatavyaadhi - Kampa                                                77   137.1     26.0   238.90     1   1427
-Total   OTHER   V2.34.1   Vaatavyaadhi - Kampa - Baahu Kampa                                   3    84.3     97.0    77.78     1    155
F        OTHER   V2.34.1   Vaatavyaadhi - Kampa - Baahu Kampa                                   2    78.0     78.0   108.89     1    155
M        OTHER   V2.34.1   Vaatavyaadhi - Kampa - Baahu Kampa                                   1    97.0     97.0       NA    97     97
-Total   OTHER   V2.34.2   Vaatavyaadhi - Kampa - Greevaa Kampa                                 1    10.0     10.0       NA    10     10
M        OTHER   V2.34.2   Vaatavyaadhi - Kampa - Greevaa Kampa                                 1    10.0     10.0       NA    10     10
-Total   OTHER   V2.35   Vaatavyaadhi - Karnashoola                                             1     7.0      7.0       NA     7      7
F        OTHER   V2.35   Vaatavyaadhi - Karnashoola                                             1     7.0      7.0       NA     7      7
-Total   OTHER   V2.38   Vaatavyaadhi - Khanja                                                 10   110.2      5.5   257.68     1    828
F        OTHER   V2.38   Vaatavyaadhi - Khanja                                                  4    22.0      5.5    36.25     1     76
M        OTHER   V2.38   Vaatavyaadhi - Khanja                                                  6   169.0      9.5   329.17     1    828
-Total   OTHER   V2.4.1   Vaatavyaadhi - Aavritavaata - Kaphaavrita Vaata                       6    98.0     64.5   120.03     1    295
F        OTHER   V2.4.1   Vaatavyaadhi - Aavritavaata - Kaphaavrita Vaata                       5    58.6      1.0    79.78     1    162
M        OTHER   V2.4.1   Vaatavyaadhi - Aavritavaata - Kaphaavrita Vaata                       1   295.0    295.0       NA   295    295
-Total   OTHER   V2.4.2   Vaatavyaadhi - Aavritavaata - Pittaavrita Vaata                       2     1.0      1.0     0.00     1      1
F        OTHER   V2.4.2   Vaatavyaadhi - Aavritavaata - Pittaavrita Vaata                       1     1.0      1.0       NA     1      1
M        OTHER   V2.4.2   Vaatavyaadhi - Aavritavaata - Pittaavrita Vaata                       1     1.0      1.0       NA     1      1
-Total   OTHER   V2.40   Vaatavyaadhi - Kroshtukasheersha                                       3    71.0     15.0   109.34     1    197
F        OTHER   V2.40   Vaatavyaadhi - Kroshtukasheersha                                       1    15.0     15.0       NA    15     15
M        OTHER   V2.40   Vaatavyaadhi - Kroshtukasheersha                                       2    99.0     99.0   138.59     1    197
-Total   OTHER   V2.49   Vaatavyaadhi - Minmin                                                  2     1.0      1.0     0.00     1      1
F        OTHER   V2.49   Vaatavyaadhi - Minmin                                                  1     1.0      1.0       NA     1      1
M        OTHER   V2.49   Vaatavyaadhi - Minmin                                                  1     1.0      1.0       NA     1      1
-Total   OTHER   V2.51   Vaatavyaadhi - Naanaatmaja                                             1     1.0      1.0       NA     1      1
F        OTHER   V2.51   Vaatavyaadhi - Naanaatmaja                                             1     1.0      1.0       NA     1      1
-Total   OTHER   V2.53   Vaatavyaadhi - Paadadaaha                                              9    47.3      1.0    85.95     1    227
F        OTHER   V2.53   Vaatavyaadhi - Paadadaaha                                              4   103.5     93.0   110.07     1    227
M        OTHER   V2.53   Vaatavyaadhi - Paadadaaha                                              5     2.4      1.0     3.13     1      8
-Total   OTHER   V2.54   Vaatavyaadhi - Paadaharsha                                             1     1.0      1.0       NA     1      1
M        OTHER   V2.54   Vaatavyaadhi - Paadaharsha                                             1     1.0      1.0       NA     1      1
-Total   OTHER   V2.55   Vaatavyaadhi - Paarshvagata Vaata                                      1     1.0      1.0       NA     1      1
F        OTHER   V2.55   Vaatavyaadhi - Paarshvagata Vaata                                      1     1.0      1.0       NA     1      1
-Total   OTHER   V2.56   Vaatavyaadhi - Pakvaashayasthita Vaata                                 4   149.2     32.5   256.23     1    531
F        OTHER   V2.56   Vaatavyaadhi - Pakvaashayasthita Vaata                                 2    32.5     32.5    44.55     1     64
M        OTHER   V2.56   Vaatavyaadhi - Pakvaashayasthita Vaata                                 2   266.0    266.0   374.77     1    531
-Total   OTHER   V2.56.0   Vaatavyaadhi - Pakshavadha                                         149   194.8     26.0   354.35     1   1736
F        OTHER   V2.56.0   Vaatavyaadhi - Pakshavadha                                          51   176.9     64.0   307.02     1   1658
M        OTHER   V2.56.0   Vaatavyaadhi - Pakshavadha                                          98   204.2     17.5   377.80     1   1736
-Total   OTHER   V2.56.2   Vaatavyaadhi - Pakshavadha - Pittaja                                 1     1.0      1.0       NA     1      1
M        OTHER   V2.56.2   Vaatavyaadhi - Pakshavadha - Pittaja                                 1     1.0      1.0       NA     1      1
-Total   OTHER   V2.57   Vaatavyaadhi - Pangutva                                               15   120.7      1.0   400.25     1   1565
F        OTHER   V2.57   Vaatavyaadhi - Pangutva                                                7   250.1     42.0   580.44     1   1565
M        OTHER   V2.57   Vaatavyaadhi - Pangutva                                                8     7.5      1.0    14.76     1     43
-Total   OTHER   V2.62   Vaatavyaadhi - Raktagata Vaata                                         3     1.7      1.0     1.15     1      3
F        OTHER   V2.62   Vaatavyaadhi - Raktagata Vaata                                         1     3.0      3.0       NA     3      3
M        OTHER   V2.62   Vaatavyaadhi - Raktagata Vaata                                         2     1.0      1.0     0.00     1      1
-Total   OTHER   V2.7   Vaatavyaadhi - Ardita                                                  13    98.0     21.0   162.20     1    568
F        OTHER   V2.7   Vaatavyaadhi - Ardita                                                  11    40.7     17.0    57.55     1    172
M        OTHER   V2.7   Vaatavyaadhi - Ardita                                                   2   413.0    413.0   219.20   258    568
-Total   OTHER   V2.71   Vaatavyaadhi - Toonee                                                  1    50.0     50.0       NA    50     50
F        OTHER   V2.71   Vaatavyaadhi - Toonee                                                  1    50.0     50.0       NA    50     50
-Total   OTHER   V2.8   Vaatavyaadhi - Ashtheelaa                                              25    20.1      1.0    62.13     1    312
M        OTHER   V2.8   Vaatavyaadhi - Ashtheelaa                                              25    20.1      1.0    62.13     1    312
-Total   OTHER   V3.0   Vandhyatava                                                            19    65.5      9.0   129.86     1    554
F        OTHER   V3.0   Vandhyatava                                                            16    66.7      8.0   140.03     1    554
M        OTHER   V3.0   Vandhyatava                                                             3    59.3     43.0    67.99     1    134
-Total   OTHER   V3.1   Vandhyatava - Kaka Vandya                                               7    42.0     15.0    69.08     1    197
F        OTHER   V3.1   Vandhyatava - Kaka Vandya                                               7    42.0     15.0    69.08     1    197
-Total   OTHER   V3.2   Vandhyatava - Vandya                                                    3    57.7      1.0    98.15     1    171
F        OTHER   V3.2   Vandhyatava - Vandya                                                    3    57.7      1.0    98.15     1    171
-Total   OTHER   V3.3   VANDHYATWA - ANAPATYA                                                   5    26.2      1.0    43.73     1    102
F        OTHER   V3.3   VANDHYATWA - ANAPATYA                                                   4    32.5     13.5    47.81     1    102
M        OTHER   V3.3   VANDHYATWA - ANAPATYA                                                   1     1.0      1.0       NA     1      1
-Total   OTHER   V5.0   Vibandha                                                               55    38.4      1.0    74.24     1    345
F        OTHER   V5.0   Vibandha                                                               26    29.9      1.0    72.90     1    345
M        OTHER   V5.0   Vibandha                                                               29    46.0      9.0    75.89     1    298
-Total   OTHER   V5.1   Vibandha - Mala Vibandha                                               77    92.5      5.0   226.17     1   1370
F        OTHER   V5.1   Vibandha - Mala Vibandha                                               35    70.9      5.0   149.65     1    612
M        OTHER   V5.1   Vibandha - Mala Vibandha                                               42   110.5      5.5   274.81     1   1370
-Total   OTHER   V5.2   Vibandha - Mootra Vibandha                                              2     2.5      2.5     0.71     2      3
F        OTHER   V5.2   Vibandha - Mootra Vibandha                                              1     2.0      2.0       NA     2      2
M        OTHER   V5.2   Vibandha - Mootra Vibandha                                              1     3.0      3.0       NA     3      3
-Total   OTHER   V6.0   Vidradhi                                                               22    41.5      1.0    84.21     1    327
F        OTHER   V6.0   Vidradhi                                                                9    72.9     12.0   114.57     1    327
M        OTHER   V6.0   Vidradhi                                                               13    19.7      1.0    49.04     1    173
-Total   OTHER   V6.11   Vidradhi - Majjajaata                                                  1    38.0     38.0       NA    38     38
M        OTHER   V6.11   Vidradhi - Majjajaata                                                  1    38.0     38.0       NA    38     38
-Total   OTHER   V6.17   Vidradhi - Stanagata                                                   1     1.0      1.0       NA     1      1
F        OTHER   V6.17   Vidradhi - Stanagata                                                   1     1.0      1.0       NA     1      1
-Total   OTHER   V6.18   Vidradhi - Udaragata                                                   1     1.0      1.0       NA     1      1
F        OTHER   V6.18   Vidradhi - Udaragata                                                   1     1.0      1.0       NA     1      1
-Total   OTHER   V6.2   Vidradhi - Asthigata                                                    1     1.0      1.0       NA     1      1
M        OTHER   V6.2   Vidradhi - Asthigata                                                    1     1.0      1.0       NA     1      1
-Total   OTHER   V6.21   Vidradhi - Vrikkagata                                                  1    71.0     71.0       NA    71     71
M        OTHER   V6.21   Vidradhi - Vrikkagata                                                  1    71.0     71.0       NA    71     71
-Total   OTHER   V6.3   Vidradhi - Baahya                                                       3   129.0      1.0   221.70     1    385
M        OTHER   V6.3   Vidradhi - Baahya                                                       3   129.0      1.0   221.70     1    385
-Total   OTHER   V9.0   Visarpa                                                                17   118.9     32.0   244.01     1    960
F        OTHER   V9.0   Visarpa                                                                 9   208.2     41.0   314.21     1    960
M        OTHER   V9.0   Visarpa                                                                 8    18.4      1.0    38.19     1    109
-Total   OTHER   Y1   Yakrit Dosha                                                             51   143.0     15.0   328.26     1   1712
F        OTHER   Y1   Yakrit Dosha                                                              8    96.8      8.0   128.45     1    258
M        OTHER   Y1   Yakrit Dosha                                                             43   151.7     18.0   353.61     1   1712
-Total   OTHER   Y2   Yoni Kandu                                                                2     6.0      6.0     7.07     1     11
F        OTHER   Y2   Yoni Kandu                                                                2     6.0      6.0     7.07     1     11
-Total   OTHER   Y8.0   Yonivyaapad                                                             3     1.0      1.0     0.00     1      1
F        OTHER   Y8.0   Yonivyaapad                                                             3     1.0      1.0     0.00     1      1
-Total   OTHER   Y8.10   Yonivyaapad - Karninee                                                 5    29.8      1.0    64.40     1    145
F        OTHER   Y8.10   Yonivyaapad - Karninee                                                 5    29.8      1.0    64.40     1    145
-Total   OTHER   Y8.12   Yonivyaapad - Mahaayoni                                                1     1.0      1.0       NA     1      1
F        OTHER   Y8.12   Yonivyaapad - Mahaayoni                                                1     1.0      1.0       NA     1      1
-Total   OTHER   Y8.13   Yonivyaapad - Pariplutaa                                              12    15.7      2.5    39.35     1    140
F        OTHER   Y8.13   Yonivyaapad - Pariplutaa                                              12    15.7      2.5    39.35     1    140
-Total   OTHER   Y8.14   Yonivyaapad - Phalinee / Andalee                                       1     3.0      3.0       NA     3      3
F        OTHER   Y8.14   Yonivyaapad - Phalinee / Andalee                                       1     3.0      3.0       NA     3      3
-Total   OTHER   Y8.15   Yonivyaapad - Pittalaa                                                 2     1.0      1.0     0.00     1      1
F        OTHER   Y8.15   Yonivyaapad - Pittalaa                                                 2     1.0      1.0     0.00     1      1
-Total   OTHER   Y8.22   Yonivyaapad - Sramsinee                                                5    20.2      6.0    29.27     1     70
F        OTHER   Y8.22   Yonivyaapad - Sramsinee                                                5    20.2      6.0    29.27     1     70
-Total   OTHER   Y8.23   Yonivyaapad - Udaavartaa                                               2     1.0      1.0     0.00     1      1
F        OTHER   Y8.23   Yonivyaapad - Udaavartaa                                               2     1.0      1.0     0.00     1      1
-Total   OTHER   Y8.28   Yonivyaapad - Viplutaa                                                 1     3.0      3.0       NA     3      3
F        OTHER   Y8.28   Yonivyaapad - Viplutaa                                                 1     3.0      3.0       NA     3      3
-Total   OTHER   Y9.0   Yuvana pidika                                                           3    10.3      1.0    16.17     1     29
F        OTHER   Y9.0   Yuvana pidika                                                           2    15.0     15.0    19.80     1     29
M        OTHER   Y9.0   Yuvana pidika                                                           1     1.0      1.0       NA     1      1
-Total   OTHER   k11.0   Kukshi Roga                                                            1     1.0      1.0       NA     1      1
M        OTHER   k11.0   Kukshi Roga                                                            1     1.0      1.0       NA     1      1
-Total   OTHER   k12.0   Kushtha                                                              188   176.0      5.0   364.98     1   1920
F        OTHER   k12.0   Kushtha                                                              100   211.3     11.5   412.79     1   1920
M        OTHER   k12.0   Kushtha                                                               88   135.9      1.0   298.94     1   1481
-Total   OTHER   k12.1   Kushtha-alasaka                                                        1     1.0      1.0       NA     1      1
M        OTHER   k12.1   Kushtha-alasaka                                                        1     1.0      1.0       NA     1      1
-Total   OTHER   k12.10   Kushtha-mandala                                                       3     5.7      1.0     8.08     1     15
F        OTHER   k12.10   Kushtha-mandala                                                       2     8.0      8.0     9.90     1     15
M        OTHER   k12.10   Kushtha-mandala                                                       1     1.0      1.0       NA     1      1
-Total   OTHER   k12.12   Kushtha-pundarika                                                     1     1.0      1.0       NA     1      1
F        OTHER   k12.12   Kushtha-pundarika                                                     1     1.0      1.0       NA     1      1
-Total   OTHER   k12.15   Kushtha-sidhma                                                        3    11.0      1.0    17.32     1     31
F        OTHER   k12.15   Kushtha-sidhma                                                        2    16.0     16.0    21.21     1     31
M        OTHER   k12.15   Kushtha-sidhma                                                        1     1.0      1.0       NA     1      1
-Total   OTHER   k12.16   Kushtha-vicharchika                                                  32    46.8      1.0    90.08     1    348
F        OTHER   k12.16   Kushtha-vicharchika                                                  20    31.6      1.0    74.95     1    291
M        OTHER   k12.16   Kushtha-vicharchika                                                  12    71.9     11.5   109.82     1    348
-Total   OTHER   k12.17   Kushtha-vipadika                                                     17   138.1      1.0   307.09     1   1166
F        OTHER   k12.17   Kushtha-vipadika                                                     11   139.8      1.0   344.88     1   1166
M        OTHER   k12.17   Kushtha-vipadika                                                      6   134.8      1.0   252.73     1    631
-Total   OTHER   k12.18   Kushtha-visphotaka                                                    1   123.0    123.0       NA   123    123
F        OTHER   k12.18   Kushtha-visphotaka                                                    1   123.0    123.0       NA   123    123
-Total   OTHER   k12.2   Kushtha-Audumbara                                                      1    65.0     65.0       NA    65     65
F        OTHER   k12.2   Kushtha-Audumbara                                                      1    65.0     65.0       NA    65     65
-Total   OTHER   k12.3   Kushtha-charmadala                                                     5    16.0      1.0    33.54     1     76
M        OTHER   k12.3   Kushtha-charmadala                                                     5    16.0      1.0    33.54     1     76
-Total   OTHER   k12.4   Kushtha-charmakhya                                                     1     1.0      1.0       NA     1      1
M        OTHER   k12.4   Kushtha-charmakhya                                                     1     1.0      1.0       NA     1      1
-Total   OTHER   k12.5   Kushtha-dadru                                                         21    56.3      1.0   167.73     1    753
F        OTHER   k12.5   Kushtha-dadru                                                          8    50.5      9.0    77.14     1    185
M        OTHER   k12.5   Kushtha-dadru                                                         13    59.9      1.0   208.28     1    753
-Total   OTHER   k12.6   Kushtha-ekakushta                                                      3   158.3      1.0   272.51     1    473
F        OTHER   k12.6   Kushtha-ekakushta                                                      2   237.0    237.0   333.75     1    473
M        OTHER   k12.6   Kushtha-ekakushta                                                      1     1.0      1.0       NA     1      1
-Total   OTHER   k12.9   Kushtha-kitibha                                                       57   123.3      6.0   274.52     1   1157
F        OTHER   k12.9   Kushtha-kitibha                                                       21    71.2     10.0   124.96     1    383
M        OTHER   k12.9   Kushtha-kitibha                                                       36   153.7      4.5   330.27     1   1157
-Total   OTHER   k8.0   Kshaya                                                                  9    11.4      1.0    21.44     1     59
F        OTHER   k8.0   Kshaya                                                                  4    10.0      1.0    18.00     1     37
M        OTHER   k8.0   Kshaya                                                                  5    12.6      1.0    25.94     1     59
-Total   OTHER   sandhigata vaa                                                                 1     1.0      1.0       NA     1      1
F        OTHER   sandhigata vaa                                                                 1     1.0      1.0       NA     1      1
-Total   RMSD   A2.0   Aamavaata                                                              832   134.1     10.0   281.64     1   1962
F        RMSD   A2.0   Aamavaata                                                              652   136.4     13.0   282.25     1   1938
M        RMSD   A2.0   Aamavaata                                                              180   125.7      1.0   280.01     1   1962
-Total   RMSD   A2.1   Aamavaata - Kaphaja                                                     17    24.8      1.0    69.78     1    286
F        RMSD   A2.1   Aamavaata - Kaphaja                                                     13    31.5      1.0    79.23     1    286
M        RMSD   A2.1   Aamavaata - Kaphaja                                                      4     2.8      1.0     3.50     1      8
-Total   RMSD   A2.2   Aamavaata - Pittaja                                                      7    18.1      4.0    33.64     1     93
F        RMSD   A2.2   Aamavaata - Pittaja                                                      4    24.8      2.5    45.52     1     93
M        RMSD   A2.2   Aamavaata - Pittaja                                                      3     9.3      8.0     9.07     1     19
-Total   RMSD   A2.3   Aamavaata - Vaataja                                                     34    26.2      1.0    66.30     1    289
F        RMSD   A2.3   Aamavaata - Vaataja                                                     27    16.4      1.0    49.00     1    201
M        RMSD   A2.3   Aamavaata - Vaataja                                                      7    64.0     10.0   108.06     1    289
-Total   RMSD   A3.0   Abhighataja Shoola                                                     739    23.6      1.0    80.25     1    734
F        RMSD   A3.0   Abhighataja Shoola                                                     331    31.5      1.0    96.96     1    730
M        RMSD   A3.0   Abhighataja Shoola                                                     408    17.1      1.0    62.94     1    734
-Total   RMSD   S10.0   Stambha                                                                 2     1.0      1.0     0.00     1      1
M        RMSD   S10.0   Stambha                                                                 2     1.0      1.0     0.00     1      1
-Total   RMSD   S10.1   Stambha - Baahu Stambha                                                 7     2.6      1.0     3.74     1     11
F        RMSD   S10.1   Stambha - Baahu Stambha                                                 4     1.2      1.0     0.50     1      2
M        RMSD   S10.1   Stambha - Baahu Stambha                                                 3     4.3      1.0     5.77     1     11
-Total   RMSD   S10.10   Stambha - Prishtha Stambha                                             2     1.0      1.0     0.00     1      1
F        RMSD   S10.10   Stambha - Prishtha Stambha                                             1     1.0      1.0       NA     1      1
M        RMSD   S10.10   Stambha - Prishtha Stambha                                             1     1.0      1.0       NA     1      1
-Total   RMSD   S10.12   Stambha - Sandhi Stambha                                               3     4.0      1.0     5.20     1     10
F        RMSD   S10.12   Stambha - Sandhi Stambha                                               1     1.0      1.0       NA     1      1
M        RMSD   S10.12   Stambha - Sandhi Stambha                                               2     5.5      5.5     6.36     1     10
-Total   RMSD   S10.13   Stambha - Siraa Stambha                                                1     1.0      1.0       NA     1      1
F        RMSD   S10.13   Stambha - Siraa Stambha                                                1     1.0      1.0       NA     1      1
-Total   RMSD   S10.14   Stambha - Uru Stambha                                                  3     6.3      1.0     9.24     1     17
F        RMSD   S10.14   Stambha - Uru Stambha                                                  3     6.3      1.0     9.24     1     17
-Total   RMSD   S10.4   Stambha - Greevaa Stambha                                              38    19.5      1.0    50.24     1    257
F        RMSD   S10.4   Stambha - Greevaa Stambha                                              18    12.7      5.0    28.11     1    123
M        RMSD   S10.4   Stambha - Greevaa Stambha                                              20    25.7      1.0    64.22     1    257
-Total   RMSD   S10.5   Stambha - Hanu Stambha                                                  4    20.5     14.0    24.89     1     53
F        RMSD   S10.5   Stambha - Hanu Stambha                                                  1     1.0      1.0       NA     1      1
M        RMSD   S10.5   Stambha - Hanu Stambha                                                  3    27.0     27.0    26.00     1     53
-Total   RMSD   S10.6   Stambha - Hridaya Stambha                                               1     1.0      1.0       NA     1      1
M        RMSD   S10.6   Stambha - Hridaya Stambha                                               1     1.0      1.0       NA     1      1
-Total   RMSD   S13.0   Sthaanabhedena Graha                                                   15    17.8      1.0    46.74     1    184
F        RMSD   S13.0   Sthaanabhedena Graha                                                    6     6.2      1.0     8.73     1     22
M        RMSD   S13.0   Sthaanabhedena Graha                                                    9    25.6      1.0    60.06     1    184
-Total   RMSD   S13.1   Sthaanabhedena Graha - Anga Graha                                       4    21.5      1.0    41.00     1     83
F        RMSD   S13.1   Sthaanabhedena Graha - Anga Graha                                       1     1.0      1.0       NA     1      1
M        RMSD   S13.1   Sthaanabhedena Graha - Anga Graha                                       3    28.3      1.0    47.34     1     83
-Total   RMSD   S13.11   Sthaanabhedena Graha - Katee Graha                                   982    56.9      1.0   171.56     1   1620
F        RMSD   S13.11   Sthaanabhedena Graha - Katee Graha                                   468    61.9      1.0   187.67     1   1620
M        RMSD   S13.11   Sthaanabhedena Graha - Katee Graha                                   514    52.4      1.0   155.50     1   1303
-Total   RMSD   S13.13   Sthaanabhedena Graha - Manyaa Graha                                   23     1.0      1.0     0.00     1      1
F        RMSD   S13.13   Sthaanabhedena Graha - Manyaa Graha                                   16     1.0      1.0     0.00     1      1
M        RMSD   S13.13   Sthaanabhedena Graha - Manyaa Graha                                    7     1.0      1.0     0.00     1      1
-Total   RMSD   S13.14   Sthaanabhedena Graha - Marma Graha                                     3     1.0      1.0     0.00     1      1
F        RMSD   S13.14   Sthaanabhedena Graha - Marma Graha                                     2     1.0      1.0     0.00     1      1
M        RMSD   S13.14   Sthaanabhedena Graha - Marma Graha                                     1     1.0      1.0       NA     1      1
-Total   RMSD   S13.17   Sthaanabhedena Graha - Paada Graha                                     3     1.0      1.0     0.00     1      1
F        RMSD   S13.17   Sthaanabhedena Graha - Paada Graha                                     3     1.0      1.0     0.00     1      1
-Total   RMSD   S13.18   Sthaanabhedena Graha - Paarshva Graha                                  3    13.0      8.0    15.13     1     30
M        RMSD   S13.18   Sthaanabhedena Graha - Paarshva Graha                                  3    13.0      8.0    15.13     1     30
-Total   RMSD   S13.19   Sthaanabhedena Graha - Prishtha Graha                                121    61.0      1.0   165.23     1    922
F        RMSD   S13.19   Sthaanabhedena Graha - Prishtha Graha                                 50    69.3      5.5   159.25     1    908
M        RMSD   S13.19   Sthaanabhedena Graha - Prishtha Graha                                 71    55.2      1.0   170.20     1    922
-Total   RMSD   S13.20   Sthaanabhedena Graha - Shiro Graha                                     1     1.0      1.0       NA     1      1
M        RMSD   S13.20   Sthaanabhedena Graha - Shiro Graha                                     1     1.0      1.0       NA     1      1
-Total   RMSD   S13.22   Sthaanabhedena Graha - Uro Graha                                       1     1.0      1.0       NA     1      1
M        RMSD   S13.22   Sthaanabhedena Graha - Uro Graha                                       1     1.0      1.0       NA     1      1
-Total   RMSD   S13.23   Sthaanabhedena Graha - Vaak Graha                                     12   152.0     59.0   214.87     1    728
F        RMSD   S13.23   Sthaanabhedena Graha - Vaak Graha                                      3   178.7    260.0   154.05     1    275
M        RMSD   S13.23   Sthaanabhedena Graha - Vaak Graha                                      9   143.1     10.0   239.16     1    728
-Total   RMSD   S13.3   Sthaanabhedena Graha - Gala Graha                                       4     1.0      1.0     0.00     1      1
F        RMSD   S13.3   Sthaanabhedena Graha - Gala Graha                                       3     1.0      1.0     0.00     1      1
M        RMSD   S13.3   Sthaanabhedena Graha - Gala Graha                                       1     1.0      1.0       NA     1      1
-Total   RMSD   S13.5   Sthaanabhedena Graha - Hanu Graha                                       1     1.0      1.0       NA     1      1
F        RMSD   S13.5   Sthaanabhedena Graha - Hanu Graha                                       1     1.0      1.0       NA     1      1
-Total   RMSD   S13.6   Sthaanabhedena Graha - Hrid Graha                                       1     1.0      1.0       NA     1      1
M        RMSD   S13.6   Sthaanabhedena Graha - Hrid Graha                                       1     1.0      1.0       NA     1      1
-Total   RMSD   S13.7   Sthaanabhedena Graha - Jaanugraha                                       5     4.2      1.0     6.10     1     15
F        RMSD   S13.7   Sthaanabhedena Graha - Jaanugraha                                       4     4.5      1.0     7.00     1     15
M        RMSD   S13.7   Sthaanabhedena Graha - Jaanugraha                                       1     3.0      3.0       NA     3      3
-Total   RMSD   S13.8   Sthaanabhedena Graha - Janghaa Graha                                    2     1.0      1.0     0.00     1      1
M        RMSD   S13.8   Sthaanabhedena Graha - Janghaa Graha                                    2     1.0      1.0     0.00     1      1
-Total   RMSD   S14.0   Sthaanabhedena Shoola                                                 523    33.3      1.0   115.78     1   1621
F        RMSD   S14.0   Sthaanabhedena Shoola                                                 289    44.0      1.0   139.86     1   1621
M        RMSD   S14.0   Sthaanabhedena Shoola                                                 234    20.1      1.0    74.40     1    777
-Total   RMSD   S14.11   Sthaanabhedena Shoola - Guda Shoola                                    1     1.0      1.0       NA     1      1
M        RMSD   S14.11   Sthaanabhedena Shoola - Guda Shoola                                    1     1.0      1.0       NA     1      1
-Total   RMSD   S14.13   Sthaanabhedena Shoola - Gulpha Shoola                                 48    29.1      1.0    87.93     1    521
F        RMSD   S14.13   Sthaanabhedena Shoola - Gulpha Shoola                                 30    26.9      1.0    61.06     1    301
M        RMSD   S14.13   Sthaanabhedena Shoola - Gulpha Shoola                                 18    32.7      1.0   122.45     1    521
-Total   RMSD   S14.14   Sthaanabhedena Shoola - Hanu Shoola                                    1     1.0      1.0       NA     1      1
M        RMSD   S14.14   Sthaanabhedena Shoola - Hanu Shoola                                    1     1.0      1.0       NA     1      1
-Total   RMSD   S14.15   Sthaanabhedena Shoola - Hasta Shoola                                   9    21.6      1.0    53.58     1    163
F        RMSD   S14.15   Sthaanabhedena Shoola - Hasta Shoola                                   9    21.6      1.0    53.58     1    163
-Total   RMSD   S14.16   Sthaanabhedena Shoola - Hrid Shoola                                   10    41.4      1.0   109.11     1    350
F        RMSD   S14.16   Sthaanabhedena Shoola - Hrid Shoola                                    2    19.0     19.0    25.46     1     37
M        RMSD   S14.16   Sthaanabhedena Shoola - Hrid Shoola                                    8    47.0      1.0   122.61     1    350
-Total   RMSD   S14.17   Sthaanabhedena Shoola - Jaanu Shoola                                 206    17.4      1.0    74.46     1    859
F        RMSD   S14.17   Sthaanabhedena Shoola - Jaanu Shoola                                 108    24.7      1.0    97.85     1    859
M        RMSD   S14.17   Sthaanabhedena Shoola - Jaanu Shoola                                  98     9.5      1.0    32.15     1    211
-Total   RMSD   S14.18   Sthaanabhedena Shoola - Janghaa Shoola                                34    12.4      1.0    36.22     1    200
F        RMSD   S14.18   Sthaanabhedena Shoola - Janghaa Shoola                                20    17.2      1.0    46.52     1    200
M        RMSD   S14.18   Sthaanabhedena Shoola - Janghaa Shoola                                14     5.5      1.0     9.03     1     28
-Total   RMSD   S14.19   Sthaanabhedena Shoola - Kantha Shoola                                  2     1.5      1.5     0.71     1      2
F        RMSD   S14.19   Sthaanabhedena Shoola - Kantha Shoola                                  1     2.0      2.0       NA     2      2
M        RMSD   S14.19   Sthaanabhedena Shoola - Kantha Shoola                                  1     1.0      1.0       NA     1      1
-Total   RMSD   S14.21   Sthaanabhedena Shoola - Katee Shoola                                1333    44.2      1.0   148.98     1   1620
F        RMSD   S14.21   Sthaanabhedena Shoola - Katee Shoola                                 689    48.5      1.0   153.88     1   1421
M        RMSD   S14.21   Sthaanabhedena Shoola - Katee Shoola                                 644    39.6      1.0   143.53     1   1620
-Total   RMSD   S14.23   Sthaanabhedena Shoola - Kukshi Shoola                                 27    29.9      1.0    67.54     1    283
F        RMSD   S14.23   Sthaanabhedena Shoola - Kukshi Shoola                                  9    78.9     40.0   101.56     1    283
M        RMSD   S14.23   Sthaanabhedena Shoola - Kukshi Shoola                                 18     5.4      1.0    14.81     1     62
-Total   RMSD   S14.24   Sthaanabhedena Shoola - Manyaa Shoola                                 30     7.7      1.0    16.91     1     80
F        RMSD   S14.24   Sthaanabhedena Shoola - Manyaa Shoola                                 14     8.3      1.0    13.25     1     43
M        RMSD   S14.24   Sthaanabhedena Shoola - Manyaa Shoola                                 16     7.1      1.0    20.00     1     80
-Total   RMSD   S14.3   Sthaanabhedena Shoola - Amsa Shoola                                    82    18.8      1.0    69.87     1    566
F        RMSD   S14.3   Sthaanabhedena Shoola - Amsa Shoola                                    47    13.4      1.0    30.61     1    133
M        RMSD   S14.3   Sthaanabhedena Shoola - Amsa Shoola                                    35    26.0      1.0   101.34     1    566
-Total   RMSD   S14.4   Sthaanabhedena Shoola - Anga Shoola                                    69    47.1      1.0   135.17     1    780
F        RMSD   S14.4   Sthaanabhedena Shoola - Anga Shoola                                    43    59.5      1.0   152.59     1    780
M        RMSD   S14.4   Sthaanabhedena Shoola - Anga Shoola                                    26    26.6      1.0    99.42     1    506
-Total   RMSD   S14.5   Sthaanabhedena Shoola - Anguli Shoola                                   7     1.0      1.0     0.00     1      1
F        RMSD   S14.5   Sthaanabhedena Shoola - Anguli Shoola                                   4     1.0      1.0     0.00     1      1
M        RMSD   S14.5   Sthaanabhedena Shoola - Anguli Shoola                                   3     1.0      1.0     0.00     1      1
-Total   RMSD   S14.6   Sthaanabhedena Shoola - Asthi Shoola                                   39     7.4      1.0    20.47     1    108
F        RMSD   S14.6   Sthaanabhedena Shoola - Asthi Shoola                                   21    12.7      1.0    27.05     1    108
M        RMSD   S14.6   Sthaanabhedena Shoola - Asthi Shoola                                   18     1.2      1.0     0.73     1      4
-Total   RMSD   S14.7   Sthaanabhedena Shoola - Baahu Shoola                                  233    41.2      1.0   111.43     1   1020
F        RMSD   S14.7   Sthaanabhedena Shoola - Baahu Shoola                                  114    51.4      1.0   118.04     1    686
M        RMSD   S14.7   Sthaanabhedena Shoola - Baahu Shoola                                  119    31.4      1.0   104.27     1   1020
-Total   RMSD   S15.28   Sthaanabhedena Shoola - Nakha Shoola                                   2     1.5      1.5     0.71     1      2
F        RMSD   S15.28   Sthaanabhedena Shoola - Nakha Shoola                                   1     2.0      2.0       NA     2      2
M        RMSD   S15.28   Sthaanabhedena Shoola - Nakha Shoola                                   1     1.0      1.0       NA     1      1
-Total   RMSD   S15.31   Sthaanabhedena Shoola - Paada Shoola                                  89    63.9      1.0   213.89     1   1390
F        RMSD   S15.31   Sthaanabhedena Shoola - Paada Shoola                                  52    59.9      1.0   191.20     1   1093
M        RMSD   S15.31   Sthaanabhedena Shoola - Paada Shoola                                  37    69.6      1.0   244.92     1   1390
-Total   RMSD   S15.32   Sthaanabhedena Shoola - Paarshni Shoola                                4     2.0      1.0     2.00     1      5
F        RMSD   S15.32   Sthaanabhedena Shoola - Paarshni Shoola                                3     2.3      1.0     2.31     1      5
M        RMSD   S15.32   Sthaanabhedena Shoola - Paarshni Shoola                                1     1.0      1.0       NA     1      1
-Total   RMSD   S15.34   Sthaanabhedena Shoola - Parva Shoola                                   2     1.0      1.0     0.00     1      1
F        RMSD   S15.34   Sthaanabhedena Shoola - Parva Shoola                                   2     1.0      1.0     0.00     1      1
-Total   RMSD   S15.36   Sthaanabhedena Shoola - Prishtha Shoola                               77    64.8      1.0   226.57     1   1310
F        RMSD   S15.36   Sthaanabhedena Shoola - Prishtha Shoola                               39    95.6      1.0   294.21     1   1310
M        RMSD   S15.36   Sthaanabhedena Shoola - Prishtha Shoola                               38    33.2      1.0   120.51     1    694
-Total   RMSD   S15.41   Sthaanabhedena Shoola - Sakthi Shoola                                  6    41.3      4.5    60.85     1    136
F        RMSD   S15.41   Sthaanabhedena Shoola - Sakthi Shoola                                  1   136.0    136.0       NA   136    136
M        RMSD   S15.41   Sthaanabhedena Shoola - Sakthi Shoola                                  5    22.4      1.0    44.04     1    101
-Total   RMSD   S15.42   Sthaanabhedena Shoola - Sandhi Shoola                                 75     8.3      1.0    25.27     1    146
F        RMSD   S15.42   Sthaanabhedena Shoola - Sandhi Shoola                                 42     9.3      1.0    24.98     1    144
M        RMSD   S15.42   Sthaanabhedena Shoola - Sandhi Shoola                                 33     7.1      1.0    25.97     1    146
-Total   RMSD   S15.43   Sthaanabhedena Shoola - Skandha Shoola                                 2     1.0      1.0     0.00     1      1
F        RMSD   S15.43   Sthaanabhedena Shoola - Skandha Shoola                                 1     1.0      1.0       NA     1      1
M        RMSD   S15.43   Sthaanabhedena Shoola - Skandha Shoola                                 1     1.0      1.0       NA     1      1
-Total   RMSD   S15.44   Sthaanabhedena Shoola - Snaayu Shoola                                  3    30.7      1.0    51.38     1     90
F        RMSD   S15.44   Sthaanabhedena Shoola - Snaayu Shoola                                  2    45.5     45.5    62.93     1     90
M        RMSD   S15.44   Sthaanabhedena Shoola - Snaayu Shoola                                  1     1.0      1.0       NA     1      1
-Total   RMSD   S15.45   Sthaanabhedena Shoola - Sphik Shoola                                   1     1.0      1.0       NA     1      1
F        RMSD   S15.45   Sthaanabhedena Shoola - Sphik Shoola                                   1     1.0      1.0       NA     1      1
-Total   RMSD   S15.46   Sthaanabhedena Shoola - Stanaanta Shoola                               2     8.0      8.0     9.90     1     15
F        RMSD   S15.46   Sthaanabhedena Shoola - Stanaanta Shoola                               2     8.0      8.0     9.90     1     15
-Total   RMSD   S15.47   Sthaanabhedena Shoola - Trika Shoola                                   6     6.0      1.0     7.97     1     19
F        RMSD   S15.47   Sthaanabhedena Shoola - Trika Shoola                                   5     7.0      1.0     8.49     1     19
M        RMSD   S15.47   Sthaanabhedena Shoola - Trika Shoola                                   1     1.0      1.0       NA     1      1
-Total   RMSD   S15.48   Sthaanabhedena Shoola - Urah Shoola                                   11     3.6      1.0     8.74     1     30
F        RMSD   S15.48   Sthaanabhedena Shoola - Urah Shoola                                    8     4.6      1.0    10.25     1     30
M        RMSD   S15.48   Sthaanabhedena Shoola - Urah Shoola                                    3     1.0      1.0     0.00     1      1
-Total   RMSD   S1A.0   Shoola                                                                 20    58.2      1.0   165.26     1    637
F        RMSD   S1A.0   Shoola                                                                 10    70.7      1.0   199.90     1    637
M        RMSD   S1A.0   Shoola                                                                 10    45.7      1.0   131.71     1    420
-Total   RMSD   V1.0   Vaatarakta                                                             529    60.3      1.0   171.67     1   1489
F        RMSD   V1.0   Vaatarakta                                                             260    60.6      1.0   175.72     1   1489
M        RMSD   V1.0   Vaatarakta                                                             269    60.0      1.0   167.98     1   1330
-Total   RMSD   V1.1   Vaatarakta - Dvandvaja                                                   1     4.0      4.0       NA     4      4
M        RMSD   V1.1   Vaatarakta - Dvandvaja                                                   1     4.0      4.0       NA     4      4
-Total   RMSD   V1.2   Vaatarakta - Gambheera                                                  14    26.3      1.0    66.80     1    248
F        RMSD   V1.2   Vaatarakta - Gambheera                                                   9    35.9      1.0    82.47     1    248
M        RMSD   V1.2   Vaatarakta - Gambheera                                                   5     9.0      1.0    17.89     1     41
-Total   RMSD   V1.3   Vaatarakta - Kapha Vaataja                                               6     5.3      3.0     5.54     1     14
F        RMSD   V1.3   Vaatarakta - Kapha Vaataja                                               3     1.0      1.0     0.00     1      1
M        RMSD   V1.3   Vaatarakta - Kapha Vaataja                                               3     9.7     10.0     4.51     5     14
-Total   RMSD   V1.4   Vaatarakta - Kaphaadhika Vaatarakta                                      2    46.0     46.0    56.57     6     86
F        RMSD   V1.4   Vaatarakta - Kaphaadhika Vaatarakta                                      1    86.0     86.0       NA    86     86
M        RMSD   V1.4   Vaatarakta - Kaphaadhika Vaatarakta                                      1     6.0      6.0       NA     6      6
-Total   RMSD   V1.5   Vaatarakta - Pittaadhika Vaatarakta                                      7    20.1      1.0    39.37     1    106
M        RMSD   V1.5   Vaatarakta - Pittaadhika Vaatarakta                                      7    20.1      1.0    39.37     1    106
-Total   RMSD   V1.7   Vaatarakta - Uttaana                                                     5     1.0      1.0     0.00     1      1
F        RMSD   V1.7   Vaatarakta - Uttaana                                                     1     1.0      1.0       NA     1      1
M        RMSD   V1.7   Vaatarakta - Uttaana                                                     4     1.0      1.0     0.00     1      1
-Total   RMSD   V1.8   Vaatarakta - Vaata Kaphaja                                               4     6.2      5.5     6.18     1     13
F        RMSD   V1.8   Vaatarakta - Vaata Kaphaja                                               2     7.0      7.0     8.49     1     13
M        RMSD   V1.8   Vaatarakta - Vaata Kaphaja                                               2     5.5      5.5     6.36     1     10
-Total   RMSD   V1.9   Vaatarakta - Vaataadhika Vaatarakta                                      4     8.5      4.5    10.85     1     24
F        RMSD   V1.9   Vaatarakta - Vaataadhika Vaatarakta                                      1    24.0     24.0       NA    24     24
M        RMSD   V1.9   Vaatarakta - Vaataadhika Vaatarakta                                      3     3.3      1.0     4.04     1      8
         RMSD   V2.0   Vaatavyaadhi                                                             1   526.0    526.0       NA   526    526
-Total   RMSD   V2.0   Vaatavyaadhi                                                          2994    58.1      1.0   171.85     1   1758
F        RMSD   V2.0   Vaatavyaadhi                                                          1613    61.0      1.0   171.50     1   1596
M        RMSD   V2.0   Vaatavyaadhi                                                          1380    54.4      1.0   171.85     1   1758
-Total   RMSD   V2.12   Vaatavyaadhi - Stabdhagaatra                                            1    20.0     20.0       NA    20     20
M        RMSD   V2.12   Vaatavyaadhi - Stabdhagaatra                                            1    20.0     20.0       NA    20     20
-Total   RMSD   V2.16   Vaatavyaadhi - Baahugata Vaata                                         19    18.6      1.0    36.44     1    129
F        RMSD   V2.16   Vaatavyaadhi - Baahugata Vaata                                          8    13.5      1.0    24.49     1     66
M        RMSD   V2.16   Vaatavyaadhi - Baahugata Vaata                                         11    22.4      1.0    43.97     1    129
-Total   RMSD   V2.23   Vaatavyaadhi - Gridhrasee                                            2613    97.8      3.0   230.82     1   1847
F        RMSD   V2.23   Vaatavyaadhi - Gridhrasee                                            1398   107.8      7.0   241.66     1   1847
M        RMSD   V2.23   Vaatavyaadhi - Gridhrasee                                            1215    86.2      1.0   217.21     1   1843
-Total   RMSD   V2.30   Vaatavyaadhi - Jaanugata Vaata                                         92     9.5      1.0    19.14     1    111
F        RMSD   V2.30   Vaatavyaadhi - Jaanugata Vaata                                         57     7.9      1.0    15.99     1     89
M        RMSD   V2.30   Vaatavyaadhi - Jaanugata Vaata                                         35    12.1      1.0    23.41     1    111
-Total   RMSD   V2.31   Vaatavyaadhi - Janghaagata Vaata                                       12     5.0      1.0     8.62     1     31
F        RMSD   V2.31   Vaatavyaadhi - Janghaagata Vaata                                       11     5.4      1.0     8.95     1     31
M        RMSD   V2.31   Vaatavyaadhi - Janghaagata Vaata                                        1     1.0      1.0       NA     1      1
-Total   RMSD   V2.36   Vaatavyaadhi - Kateegata Vaata                                        404    31.1      1.0    97.04     1   1257
F        RMSD   V2.36   Vaatavyaadhi - Kateegata Vaata                                        195    36.7      1.0    93.70     1    627
M        RMSD   V2.36   Vaatavyaadhi - Kateegata Vaata                                        209    25.9      1.0   100.00     1   1257
-Total   RMSD   V2.42   Vaatavyaadhi - Maamsagata Vaata                                        55    76.4      1.0   251.29     1   1576
F        RMSD   V2.42   Vaatavyaadhi - Maamsagata Vaata                                        25    79.0      1.0   205.99     1    743
M        RMSD   V2.42   Vaatavyaadhi - Maamsagata Vaata                                        30    74.2      1.0   287.15     1   1576
-Total   RMSD   V2.43   Vaatavyaadhi - Maamsamedogata Vaata                                     2   281.5    281.5   396.69     1    562
F        RMSD   V2.43   Vaatavyaadhi - Maamsamedogata Vaata                                     1   562.0    562.0       NA   562    562
M        RMSD   V2.43   Vaatavyaadhi - Maamsamedogata Vaata                                     1     1.0      1.0       NA     1      1
-Total   RMSD   V2.44   Vaatavyaadhi - Majjaagata Vaata                                         6    87.8      1.0   141.33     1    330
F        RMSD   V2.44   Vaatavyaadhi - Majjaagata Vaata                                         2     1.0      1.0     0.00     1      1
M        RMSD   V2.44   Vaatavyaadhi - Majjaagata Vaata                                         4   131.2     97.0   160.46     1    330
-Total   RMSD   V2.45   Vaatavyaadhi - Majjaasthigata Vaata                                     6    22.3     24.0    13.88     5     37
F        RMSD   V2.45   Vaatavyaadhi - Majjaasthigata Vaata                                     2    24.0     24.0     7.07    19     29
M        RMSD   V2.45   Vaatavyaadhi - Majjaasthigata Vaata                                     4    21.5     22.0    17.37     5     37
-Total   RMSD   V2.46   Vaatavyaadhi - Manyaagata Vaata                                        13    20.2      1.0    50.32     1    183
F        RMSD   V2.46   Vaatavyaadhi - Manyaagata Vaata                                         2     1.0      1.0     0.00     1      1
M        RMSD   V2.46   Vaatavyaadhi - Manyaagata Vaata                                        11    23.7      1.0    54.33     1    183
-Total   RMSD   V2.47   Vaatavyaadhi - Manyaastambha                                           27    52.2      1.0   173.01     1    882
F        RMSD   V2.47   Vaatavyaadhi - Manyaastambha                                           13    35.0      1.0    68.73     1    219
M        RMSD   V2.47   Vaatavyaadhi - Manyaastambha                                           14    68.1      1.0   234.38     1    882
-Total   RMSD   V2.48   Vaatavyaadhi - Medogata Vaata                                           1     1.0      1.0       NA     1      1
M        RMSD   V2.48   Vaatavyaadhi - Medogata Vaata                                           1     1.0      1.0       NA     1      1
-Total   RMSD   V2.61   Vaatavyaadhi - Prishthagata Vaata                                      13    62.2     14.0   140.08     1    508
F        RMSD   V2.61   Vaatavyaadhi - Prishthagata Vaata                                       5     7.2      6.0     6.53     1     14
M        RMSD   V2.61   Vaatavyaadhi - Prishthagata Vaata                                       8    96.6     17.0   173.49     1    508
-Total   RMSD   V2.63   Vaatavyaadhi - Sandhigata Vaata                                      4299    77.6      1.0   208.35     1   1928
F        RMSD   V2.63   Vaatavyaadhi - Sandhigata Vaata                                      2857    86.4      1.0   220.09     1   1928
M        RMSD   V2.63   Vaatavyaadhi - Sandhigata Vaata                                      1442    60.2      1.0   181.71     1   1713
-Total   RMSD   V2.64   Vaatavyaadhi - Sarvaangagata Vaata                                     46    15.9      1.0    34.88     1    183
F        RMSD   V2.64   Vaatavyaadhi - Sarvaangagata Vaata                                     21    11.8      1.0    22.07     1     83
M        RMSD   V2.64   Vaatavyaadhi - Sarvaangagata Vaata                                     25    19.3      1.0    42.99     1    183
-Total   RMSD   V2.65   Vaatavyaadhi - Shaakhaagata Vaata                                       7     2.7      1.0     4.11     1     12
F        RMSD   V2.65   Vaatavyaadhi - Shaakhaagata Vaata                                       5     3.4      1.0     4.83     1     12
M        RMSD   V2.65   Vaatavyaadhi - Shaakhaagata Vaata                                       2     1.0      1.0     0.00     1      1
-Total   RMSD   V2.68   Vaatavyaadhi - Siraagata Vaata                                         54    34.7      1.0   125.52     1    702
F        RMSD   V2.68   Vaatavyaadhi - Siraagata Vaata                                         22    21.1      1.0    66.33     1    270
M        RMSD   V2.68   Vaatavyaadhi - Siraagata Vaata                                         32    44.0      1.0   154.07     1    702
-Total   RMSD   V2.69   Vaatavyaadhi - Siraagraha                                               1     1.0      1.0       NA     1      1
F        RMSD   V2.69   Vaatavyaadhi - Siraagraha                                               1     1.0      1.0       NA     1      1
-Total   RMSD   V2.70   Vaatavyaadhi - Snaayugata Vaata                                        19    24.6      1.0    76.69     1    337
F        RMSD   V2.70   Vaatavyaadhi - Snaayugata Vaata                                        10    37.0      1.0   105.51     1    337
M        RMSD   V2.70   Vaatavyaadhi - Snaayugata Vaata                                         9    10.8      1.0    17.36     1     43
-Total   RMSD   V2.72   Vaatavyaadhi - Trikgata Vaata                                           2     1.0      1.0     0.00     1      1
F        RMSD   V2.72   Vaatavyaadhi - Trikgata Vaata                                           1     1.0      1.0       NA     1      1
M        RMSD   V2.72   Vaatavyaadhi - Trikgata Vaata                                           1     1.0      1.0       NA     1      1
-Total   RMSD   V2.73   Vaatavyaadhi - Tvaggata Vaata                                           6     6.2      1.0     8.16     1     19
F        RMSD   V2.73   Vaatavyaadhi - Tvaggata Vaata                                           3     1.0      1.0     0.00     1      1
M        RMSD   V2.73   Vaatavyaadhi - Tvaggata Vaata                                           3    11.3     14.0     9.29     1     19
-Total   RMSD   V2.74   Vaatavyaadhi - Urugata Vaata                                            1     1.0      1.0       NA     1      1
F        RMSD   V2.74   Vaatavyaadhi - Urugata Vaata                                            1     1.0      1.0       NA     1      1
-Total   RMSD   V2.75   Vaatavyaadhi - Vaatakantaka                                           164    30.8      1.0    80.00     1    686
F        RMSD   V2.75   Vaatavyaadhi - Vaatakantaka                                           116    39.2      1.0    93.19     1    686
M        RMSD   V2.75   Vaatavyaadhi - Vaatakantaka                                            48    10.4      1.0    18.62     1     89
-Total   RMSD   V2.77   Vaatavyaadhi - Vishvaachee                                             61    28.1      1.0    94.51     1    675
F        RMSD   V2.77   Vaatavyaadhi - Vishvaachee                                             38    29.3      1.0   111.13     1    675
M        RMSD   V2.77   Vaatavyaadhi - Vishvaachee                                             23    26.2      3.0    59.87     1    257
-Total   RMSD   V2.9   Vaatavyaadhi - Asthigata Vaata                                         206    37.8      1.0   129.46     1   1230
F        RMSD   V2.9   Vaatavyaadhi - Asthigata Vaata                                         130    35.9      1.0   133.13     1   1230
M        RMSD   V2.9   Vaatavyaadhi - Asthigata Vaata                                          76    41.0      1.0   123.72     1    706



Table: Metabolic: Summary statistics of baseline age in years

 get  get.1       n   mean   median      SD   min   max
----  ------  -----  -----  -------  ------  ----  ----
   1  F        2098   46.5       47   14.56     0    88
   1  M        2349   49.4       48   13.96     5    91



Table: RMSD: Summary statistics of baseline age in years

 get  get.1       n   mean   median      SD   min   max
----  ------  -----  -----  -------  ------  ----  ----
   1  F        7935   48.8       50   14.73     0    91
   1  M        6356   47.7       46   15.91     0   101

## Including Plots

You can also embed plots, for example:

![](100_adsl_analysis_files/figure-docx/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
