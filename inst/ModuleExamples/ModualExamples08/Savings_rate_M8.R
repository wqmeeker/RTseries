#------------------------------------------------------------------------
#Example M8: Savings rate
#------------------------------------------------------------------------

library("RTseries")
# -----------------------------------------------------------------------------------

# code for figure on page 8-33
savings.rate.ts <- ts(scan(RTseriesExtDataPath("SavingsRate.txt")),start=1955,frequency=4)
savings.rate.tsd <- tsd(savings.rate.ts,data.title="US Savings Rate for 1955-1980", time.units="Year", response.units="Percent of Disposable Income")
savings.rate.tsd
iden(savings.rate.tsd)

# ----------------------------------------------------------------------------------
# code for figure on page 8-34
iden(savings.rate.tsd,d=1)
# ----------------------------------------------------------------------------------
# code for figure on page 8-35 8-36
esti(data.tsd = savings.rate.tsd, model =model.pdq(p = 1,q=2))

# ----------------------------------------------------------------------------------
# code for figure on page 8-38 8-39
esti(data.tsd = savings.rate.tsd, model =model.pdq(p = 0,d = 1,q = 1 ))

# ----------------------------------------------------------------------------------
# code for figure on page 8-40
esti(data.tsd = savings.rate.tsd, model =model.pdq(p = 1,q=2))

# ----------------------------------------------------------------------------------
# code for figure on page 8-41 8-42
esti(data.tsd = savings.rate.tsd, model =model.pdq(p = 0,d = 1,q = 2 ))

# ----------------------------------------------------------------------------------
# code for figure on page 8-43
esti(data.tsd = savings.rate.tsd, model =model.pdq(p = 0,d = 1,q = 1 ))
# ----------------------------------------------------------------------------------
