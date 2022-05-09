--Daily Tables 

Select *
From dailyCalories_merged

Select *
From dailyIntensities_merged

Select *
From dailySteps_merged

Select *
From weightLogInfo_merged

Select *
From sleepDay_merged

-- Combine Daily Data 

--Drop Time Component from Date for Sleep Table 
Select SleepDay, Replace(SleepDay, ' 12:00:00 AM', '')
From sleepDay_merged

Alter Table sleepDay_merged
Add SleepDayDate varchar(250)

Update sleepDay_merged
Set SleepDayDate = Replace(SleepDay, ' 12:00:00 AM', '')


--Make Changes to the weightLoginfo_merged 
-- Drop Time Component from Date for Weight Table and make blanks nulls 
-- Parsename: Deliminater needs to be period, hence the replace()
Select Date, 
--Left(Date, Len(Date) -12)
--,Substring(Date, 0, Len(Date) +1-12) -- Alt
--Didnt work since the time component are not a consistent 12 charaters 
	PARSENAME(Replace(Date, ' ', '.'), 3) -- Date
--	PARSENAME(Replace(Date, ' ', '.'), 2), -- Time 
--	PARSENAME(Replace(Date, ' ', '.'), 1) -- AM/PM
From weightLogInfo_merged

Alter Table weightLogInfo_merged
Add DateYMD nvarchar(255)

Update weightLogInfo_merged
Set DateYMD = Left(Date, Len(Date) -12)
Update weightLogInfo_merged
Set Fat = NULLIF(Fat, '')




Select intensity.*, cals.Calories, steps.StepTotal, sleep.TotalSleepRecords, sleep.TotalMinutesAsleep, 
	sleep.TotalTimeInBed, weightKg, WeightPounds, Fat, BMI, IsManualReport, Logid, 
	Count(*) over (Partition by intensity.Id order by intensity.Id, intensity.ActivityDay) as DayCount
From dailyCalories_merged As cals
Left Join dailyIntensities_merged as intensity
	On cals.Id = intensity.Id and cals.ActivityDay = intensity.ActivityDay
Left Join dailySteps_merged as steps
	On steps.Id = intensity.Id and steps.ActivityDay = intensity.ActivityDay
Left Join sleepDay_merged as sleep
	On steps.Id =sleep.Id and steps.ActivityDay = sleep.SleepDayDate
Left Join weightLogInfo_merged as weight
	On weight.Id =sleep.Id and weight.DateYMD = sleep.SleepDayDate

--Create Table for 
--Used the create table tool in app 

-- Copied the col names and data types after the fact 
Drop Table if exists DailyData 
CREATE TABLE [dbo].[DailyData](
	[Id] [nvarchar](50) NULL,
	[ActivityDay] [date] NULL,
	[SedentaryMinutes] [int] NULL,
	[LightlyActiveMinutes] [int] NULL,
	[FairlyActiveMinutes] [int] NULL,
	[VeryActiveMinutes] [int] NULL,
	[SedentaryActiveDistance] [float] NULL,
	[LightActiveDistance] [float] NULL,
	[ModeratelyActiveDistance] [float] NULL,
	[VeryActiveDistance] [float] NULL,
	[Calories] [int] NULL,
	[StepTotal] [int] NULL,
	[TotalSleepRecords] [int] NULL,
	[TotalMinutesAsleep] [int] NULL,
	[TotalTimeInBed] [int] NULL,
	[weightKg] [float] NULL,
	[weightPounds] [float] NULL,
	[Fat] [int] NULL,
	[BMI] [float] NULL,
	[IsManualReport] [nvarchar](50) NULL,
	[Logid] [nvarchar](50) NULL,
	[DayCount] [int] NULL
)
--Insert Join table to new table DailyData 
Insert into DailyData
Select intensity.*, cals.Calories, steps.StepTotal, sleep.TotalSleepRecords, sleep.TotalMinutesAsleep, 
	sleep.TotalTimeInBed, weightKg, WeightPounds, Fat, BMI, IsManualReport, Logid, 
	Count(*) over (Partition by intensity.Id order by intensity.Id, intensity.ActivityDay) as DayCount
From dailyCalories_merged As cals
Left Join dailyIntensities_merged as intensity
	On cals.Id = intensity.Id and cals.ActivityDay = intensity.ActivityDay
Left Join dailySteps_merged as steps
	On steps.Id = intensity.Id and steps.ActivityDay = intensity.ActivityDay
Left Join sleepDay_merged as sleep
	On steps.Id =sleep.Id and steps.ActivityDay = sleep.SleepDayDate
Left Join weightLogInfo_merged as weight
	On weight.Id =sleep.Id and weight.DateYMD = sleep.SleepDayDate

Select *
From DailyData







