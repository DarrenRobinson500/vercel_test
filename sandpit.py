from datetime import datetime, date, timedelta, time
from dateutil.relativedelta import relativedelta

def add_month(date):
    date += relativedelta(weeks=1)
    return date


note_date = datetime.today()

for x in range(12):
    note_date = add_month(note_date)
    print(note_date.date())
