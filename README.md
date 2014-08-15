risk-r-VaRwithVega
==================

#### Runs Value at Risk ####

#### Program calculates VaR at five levels -- Firm, Group, Location, Grouping, and BaseUsym ####

#### INPUTS
#### Requires two arguments -- date of the correlation file, and date of the input file (both in 'yyyymmdd' format)
#### correlation file and input file must be in respective folders 'N:\\Ned.Consultant\\VaR\\Hist\\ [yyyymmdd] \\
#### correlation file must be named [yyyymmdd] & '_mat_corr_input.csv'
#### input file must be named [yyyymmdd] & '_mat_mv_details.csv'
#### the inputs for Vega is built into the table 'mat_mv_details'; see ???? for details

#### OUTPUT FILES
#### FirmVaR - "N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_out_var_firm.csv"
#### ResultVaRGrouping - "N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_out_varm_grouping.csv"
#### ResultVaRGroup - "N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_out_varm_group.csv"
#### ResultVaRBaseUsym - "N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_out_varm_baseusym.csv"
#### ResultVaRLocation - "N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_out_varm_location.csv"
