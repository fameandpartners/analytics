echo 'Running sales.R'
Rscript Rscripts/sales.R
echo 'Running update.py'
python update.py
# python update.py production
echo 'Running daily.R'
Rscript Rscripts/board/daily.R
echo 'Running monthly_cohorts.R'
Rscript Rscripts/board/monthly_cohorts.R
python update_board.py
# python update_board.py production
