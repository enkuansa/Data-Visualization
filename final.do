* Final Exam Do File
* Economics 411
* Spring 2025

import delimited https://raw.githubusercontent.com/flynnecon/forecasting2025/refs/heads/main/final/wti_exam.csv, clear


gen sdate = tm(1986m1) + _n-1
format sdate %tm
tsset sdate