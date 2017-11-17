echo 'Running sales.R'
sudo Rscript Rscripts/sales.R
echo 'Running update.py'
#python update.py
python3 update.py production
echo 'Running monthly_cohorts.R'
sudo Rscript Rscripts/board/monthly_cohorts.R
echo 'Running daily.R'
sudo Rscript Rscripts/board/daily.R
echo 'Running monthly_cohorts.R'
sudo Rscript Rscripts/board/monthly_cohorts.R
python3 update_board.py production
# python update_board.py production
