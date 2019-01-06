# python_exercise
pip 下载： pip install -i https://pypi.tuna.tsinghua.edu.cn/simple pyspider==version_id  
clone某一个git: git clone git://github.com/numpy/numpy.git numpy    
pip install -t  D:\Python相关\Python\Lib\site-packages -i https://pypi.tuna.tsinghua.edu.cn/simple pandas==0.23.0  
克隆某一个git: git clone git://github.com/numpy/numpy.git numpy   
ggplot使用注意:C:\Users\WannaSobe\AppData\Local\Programs\Python\Python36\lib\site\packages\ggplot\stats\smoothers.py
  将from pandas.lib import Timestamp改为 from pandas import Timestamp
  * 【chapter one】[基本处理](https://github.com/w666x/python_exercise/blob/master/python_string)
    * [string.py](https://github.com/w666x/python_exercise/blob/master/python_string/string.py) is about the string such as split, join, strip, lower, replace, regx match and so on.
    * [date.py](https://github.com/w666x/python_exercise/blob/master/python_string/date.py) is about the date and datetime. Such as date, time, datetime, timedelta, strftime, strptime as so on.
    * [list.py](https://github.com/w666x/python_exercise/blob/master/python_string/list.py) is about the date and list. Such as len, max, count, reverse, sort, sorted and so on.
    * [dict.py](https://github.com/w666x/python_exercise/blob/master/python_string/dict.py) is about the dictionary. Such as key, values, items, in, not in, get, sorting and so on.
    * [control.py](https://github.com/w666x/python_exercise/blob/master/python_string/control.py) is about the exec flows. Such as for loop, while loop, if-else-if, function, try-exception and so on.
    * [simple_write.py](https://github.com/w666x/python_exercise/blob/master/python_string/simple_write.py) is about the reading data from the csv, txt and wirtting into data.

  * 【chapter two】[csv读写](https://github.com/w666x/python_exercise/blob/master/csv_file)
    * [simple_r_w](https://github.com/w666x/python_exercise/blob/master/csv_file/1csv_simple_r_w.py) is simple reading from or wirting into csv file using python function.
    * [pandas_r_w_csv](https://github.com/w666x/python_exercise/blob/master/csv_file/2csv_pandas_r_w.py)  reading from or wirting into csv file using pandas package.
    * [csv_r_w_csv](https://github.com/w666x/python_exercise/blob/master/csv_file/3csv_r_w_csv.py) is simple reading from or wirting into csv file using the csv package.
    * [r_w_csv_filter_row](https://github.com/w666x/python_exercise/blob/master/csv_file/4csv_r_w_filter_row.py)  reading from or wirting into csv some specific rows which the date meets some special conditions.
    * [r_w_csv_filter_col](https://github.com/w666x/python_exercise/blob/master/csv_file/5csv_r_w_filter_col.py)  reading from or wirting into csv some specific cols which the date meets some special conditions.
    * [filting_contiguous_rows](https://github.com/w666x/python_exercise/blob/master/csv_file/6csv_filting_contiguous_rows.py)  reading from or wirting into csv some specific cols using row_index and add column_name to the file.
    * [multiple_csvfile_r_w](https://github.com/w666x/python_exercise/blob/master/csv_file/7csv_multiple_csvfile_r_w.py)  reading from or wirting into csv basing on the multiple csv files.
    * [basic_statistics_csv_files](https://github.com/w666x/python_exercise/blob/master/csv_file/8csv_basic_statistics_csv_files.py) calculating the basic statistics outcomes basing the multiple csv files.

  * 【chapter three】[excel相关](https://github.com/w666x/python_exercise/blob/master/excel)
    * [open_excel](https://github.com/w666x/python_exercise/blob/master/excel/1excel_open_excel.py) on how to open the excel and simply calcualte the number of the worksheets and the nrow and ncol of each worksheet.
    * [single_excel_r_w](https://github.com/w666x/python_exercise/blob/master/excel/2excel_single_excel_r_w.py) on how to read and write a single excel.
    * [filter_specific_rows](https://github.com/w666x/python_exercise/blob/master/excel/3excel_filter_specific_rows.py) on how to optical filtering the specific rows from the excel file.
    * [filter_specific_cols](https://github.com/w666x/python_exercise/blob/master/excel/4excel_filter_specific_cols.py) on how to optical filtering the specific columns from the excel file.
    * [reading_all_worksheets_in_workbook](https://github.com/w666x/python_exercise/blob/master/excel/5excel_reading_all_worksheets_in_workbook.py) on reading the all worksheets in workbook and filtered specific columns and rows.
    * [count_cols_rows_of_multiple_workbook](https://github.com/w666x/python_exercise/blob/master/excel/6excel_count_cols_rows_of_multiple_workbook.py) on calculating the rows and cols of the multiple workbooks.
    * [concat_data_from_Multiple_workbooks](https://github.com/w666x/python_exercise/blob/master/excel/7excel_concat_data_from_Multiple_workbooks.py) on calculating the concatenating the data from multiple workbooks.
    * [sum_average_per_workbook](https://github.com/w666x/python_exercise/blob/master/excel/8excel_sum_average_per_workbook.py) on calculating the the sum oraverage of multiple workbook and multiple worksheets.

  * 【chapter four】[db相关](https://github.com/w666x/python_exercise/blob/master/db)
    * [count_rows](https://github.com/w666x/python_exercise/blob/master/db/1db_count_rows.py) on how to connect the database and fetch the data from the database.
    * [insert_rows](https://github.com/w666x/python_exercise/blob/master/db/2db_insert_rows.py) on how to insert data into the database from the csv file or the multiple csv files.
    * [update_rows](https://github.com/w666x/python_exercise/blob/master/db/3db_update_rows.py) on how to update the rows of the database basing on the multiple csv fles.
    * [mysql_load_from_csv](https://github.com/w666x/python_exercise/blob/master/db/4db_mysql_load_from_csv.py) on how to loading data into the mysql database from the csv files.
    * [mysql_write_to_file](https://github.com/w666x/python_exercise/blob/master/db/5db_mysql_write_to_file.py) on write the data into the csv file from the database.
    * [mysql_update_from_csv](https://github.com/w666x/python_exercise/blob/master/db/6db_mysql_update_from_csv.py) on how to updating the database basing on the csv file.

  * 【chapter 5】[应用](https://github.com/w666x/python_exercise/blob/master/application)
    * [search_for_items_write_found](https://github.com/w666x/python_exercise/blob/master/application/1search_for_items_write_found.py) is about the serach for something in somewhere and write down the outcome to output file.
    * [calculate_statistic_by_categor](https://github.com/w666x/python_exercise/blob/master/application/2calculate_statistic_by_category.py) is about the serach for something in csv file and write down the outcome to output file.  
    * [parse_text_file](https://github.com/w666x/python_exercise/blob/master/application/3parse_text_file.py) is about the serach for something in text file and write down the outcome to output file.

* 【chapter six】[figure and plots](https://github.com/w666x/python_exercise/blob/master/figure_and_plot)
  * [barplot](https://github.com/w666x/python_exercise/blob/master/figure_and_plot/1bar_plot.py) is using `matplotlib.pyplot` to create bar plot in python.
  * [histogram](https://github.com/w666x/python_exercise/blob/master/figure_and_plot/2histogram.py) is using the `pyplot` to create histogram in python.
  * [line](https://github.com/w666x/python_exercise/blob/master/figure_and_plot/3line_plot.py) is how to create the line plot in python.
  * [scatter](https://github.com/w666x/python_exercise/blob/master/figure_and_plot/4scatter_plot.py) is how to create scatter in python.
  * [boxplot](https://github.com/w666x/python_exercise/blob/master/figure_and_plot/5box_plot.py) is how to create box plot in python.
  * [pandasplot](https://github.com/w666x/python_exercise/blob/master/figure_and_plot/6pandas_plot.py) is using the pandas package to creating the plots we needed.
  * [ggplot](https://github.com/w666x/python_exercise/blob/master/figure_and_plot/7ggplot_plots.py) is using the ggplot package to create the plots and our code script doesn't work well.
  * [seaborn](https://github.com/w666x/python_exercise/blob/master/figure_and_plot/8seaborn_plot.py) is using the seaborn package to creating the plots we needed.

* 【chapter seven】[描述性统计分析和模型拟合](https://github.com/w666x/python_exercise/blob/master/descriptive_statistics)
    * [linear_regression](https://github.com/w666x/python_exercise/blob/master/descriptiove_statistics/1wine_quality.py) exploring and summarizing datasets with plots and summary statistics and conducting regression analyses with multivariate linear regression.
    * [logistic_regression](https://github.com/w666x/python_exercise/blob/master/descriptiove_statistics/2customer_churn.py) exploring and summarizing datasets with plots and summary statistics and conducting classfication analyses with logistic regression.
