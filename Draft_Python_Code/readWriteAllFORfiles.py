import glob
import os


# Path for .for files /Users/bradfritz1/Dropbox/AGDISP 829/agdisp/dll
name = 'bradfritz1'  # This is for Desktop
#name = 'bradleyfritz' # This is for laptop


file_location = os.path.join('/Users', name, 'Dropbox', 'AGDISP 829', 'agdisp', 'dll', '*.for')

file_location2 = os.path.join('/Users', name, 'Dropbox', 'AGDISP 829', 'agdisp', 'dll', '*.inc')

file_location3 = os.path.join('/Users', name, 'Dropbox', 'AGDISP 829', 'agdisp', '*.frm')

filenames = glob.glob(file_location)

filenames2 = glob.glob(file_location2)

filenames3 = glob.glob(file_location3)

# Define the variable or parameter of interest to search the files for.
var = 'IHALF'

for f in filenames:
    file = open(f, 'r')
    data = file.readlines()
    file.close()
    linenum=0
    for line in data:
        linenum = linenum+1
        if var in line:
            print(linenum, f)

for f2 in filenames2:
    file2 = open(f2, 'r')
    data2 = file2.readlines()
    file2.close()
    linenum2 = 0
    for line2 in data2:
        linenum2 = linenum2 + 1
        if var in line2:
            print(linenum2, f2)
#
# for f3 in filenames3:
#     file3 = open(f3, 'rb')
#     line = file3.readline()
#     print(file3, line)
