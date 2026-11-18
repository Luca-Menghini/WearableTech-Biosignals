# emptying the work environment
rm(list=ls())

# install and open required packages
require(pracma)

# working directory where you should paste the CSV files
# downloaded from Moodle
getwd()

###################################################
# ECG
###################################################

# reading ECG data
ecg <- read.csv("10-sec-ecg.csv")

# inspecting data file
View(ecg)
summary(ecg) # t = time (seconds from beginning), milliVolts = ecg signal

# plotting ECG signal
plot(x = ecg$t, # x-variable = time (seconds from beginning)
     y = ecg$milliVolts, # y-variable = ECG signal (milliVolts)
     type="l", # type = "l" to plot the lines
     xlab="Time (seconds)", ylab = "ECG (mV)",
     main = "10 seconds of ECG signal")

# computing sampling frequency: how many values per second?
sf <- 1 / median(diff(ecg$t))
sf # 130 Hz

# local maximum algorithm to detect R peaks
# (findpeaks function from 'pracma' package)
peaks <- as.data.frame(
  findpeaks(
    ecg$milliVolts, # find peaks in the signal (local maxima)
    minpeakdistance = round(0.5 * sf) # setting minimum distance to 500 ms
    ))

# adding detected peaks to ECG plot
points(x = ecg$t[peaks[,2]], y = peaks[,1],
       pch = 20, col = "red")

# from detected peaks to inter-beat intervals (IBIs)
IBI <- data.frame(
  # time in seconds
  time = ecg$t[sort(peaks[,2])][-1],
  # IBIs in milliseconds (multiplying by 1,000)
  IBI = diff(ecg$t[sort(peaks[,2])])*1000
)

# inspecting IBI time series
View(IBI)
summary(IBI)

# plotting IBIs (Respiratory sinuous arrhythmia?)
plot(IBI$time,
     IBI$IBI,
     type = "b",
     pch = 20,
     xlab = "Time (s)",
     ylab = "IBI (ms)",
     main = "Inter-beat intervals extracted from ECG")

# from IBIs to instantaneous heart rate (HR) in bpm
IBI$HR <- 1/IBI$IBI*60000
IBI$HR # inspecting HR time series

# plotting instantaneous HR
plot(IBI$time,
     IBI$HR,
     type = "b",
     pch = 20,
     xlab = "Time (s)",
     ylab = "HR (bpm)",
     main = "Instantaneous HR extracted from ECG")

# compute mean HR across the recording
mean(IBI$HR) # 99 bpm 

# compute SDNN across the recording
sd(IBI$IBI) # 19.0 ms

# compute RMSSD across the recording
round(sqrt(mean(diff(IBI$IBI)^2)),2) # 18.5 ms

###################################################
# PPG
###################################################

# working directory where you should paste the CSV files
# downloaded from Moodle
getwd()

# reading ECG data
ppg <- read.csv("10-sec-ppg.csv")

# inspecting data file
View(ppg)
summary(ppg) # t = time (seconds from beginning), ppg = PPG signal (blood volume pulse)

# plotting PPG signal
plot(x = ppg$t, # x-variable = time (seconds from beginning)
     y = ppg$ppg, # y-variable = PPG signal 
     type="l", # type = "l" to plot the lines
     xlab="Time (seconds)", ylab = "PPG",
     main = "10 seconds of PPG signal")

# computing sampling frequency: how many values per second?
sf <- 1 / median(diff(ppg$t))
sf # 64 Hz

# inverting the PPG signal for detecting systolic foots
ppg$ppg.inv <- -1*ppg$ppg

# plotting inverted PPG signal
plot(x = ppg$t, 
     y = ppg$ppg.inv, # y-variable = PPG signal 
     type="l", xlab="Time (seconds)", ylab = "Inverted PPG",
     main = "Inverted PPG signal")

# local maximum algorithm to detect systolic foots
# (findpeaks function from 'pracma' package)
peaks <- as.data.frame(
  findpeaks(
    ppg$ppg.inv, # find peaks in the signal (local maxima)
    minpeakdistance = round(0.5 * sf) # setting minimum distance to 500 ms
  ))

# adding detected peaks to ECG plot
points(x = ppg$t[peaks[,2]], y = peaks[,1],
       pch = 20, col = "red")

# from detected peaks to inter-pulse intervals (IPIs)
IPI <- data.frame(
  # time in seconds
  time = ppg$t[sort(peaks[,2])][-1],
  # IPIs in milliseconds (multiplying by 1,000)
  IPI = diff(ppg$t[sort(peaks[,2])])*1000
)

# inspecting IBI time series
View(IPI)
summary(IPI)

# plotting IBIs (Respiratory sinuous arrhythmia?)
plot(IPI$time,
     IPI$IPI,
     type = "b",
     pch = 20,
     xlab = "Time (s)",
     ylab = "IPI (ms)",
     main = "Inter-pulse intervals extracted from PPG")

# from IPIs to instantaneous pulse rate (PR) in bpm
IPI$PR <- 1/IPI$IPI*60000
IPI$PR # inspecting HR time series

# plotting instantaneous HR
plot(IPI$time,
     IPI$PR,
     type = "b",
     pch = 20,
     xlab = "Time (s)",
     ylab = "PR (bpm)",
     main = "Instantaneous PR extracted from PPG")

# compute mean PR across the recording
mean(IPI$PR) # 72 bpm 

# compute SDNN across the recording
sd(IPI$IPI) # 77.9 ms

# compute RMSSD across the recording
round(sqrt(mean(diff(IPI$IPI)^2)),2) # 79.4 ms


###################################################
# Assignment
###################################################

# use the following code to enable a 4-panel plot
# where you should plot the following:
# 1. ECG raw signal
# 2. IBIs extracted from ECG
# 3. PPG raw signal
# 4. IPIs extracted from PPG

par(mfrow=c(2,2))
# paste the plots here and run all of them 
# after running the previous line

# then save the generated figure and uploade it on Moodle!