echo 'Running touch_points.R'
Rscript Rscripts/touch_points.R
echo 'Running sales.R'
Rscript Rscripts/sales.R
echo 'Running update.py'
python update.py
# python update.py production
echo 'Running monthly_cohorts.R'
Rscript Rscripts/board/monthly_cohorts.R
echo 'Running daily.R'
Rscript Rscripts/board/daily.R
