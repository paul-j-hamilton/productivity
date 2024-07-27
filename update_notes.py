# -*- coding: utf-8 -*-
"""
Created on Sun Dec  3 08:49:31 2023

@author: Work
"""

import os
import sys
import pandas as pd
import shutil

script_path = os.getcwd()

# Path to the Downloads folder (adjust as needed)
downloads_path = sys.argv[1]
print(downloads_path)

# Path to the destination folder
reading_notes_path = 'G:\\My Drive\\overleaf\\Reading Notes\\'
course_notes_path = 'G:\\My Drive\\overleaf\\Course Notes\\'

crosswalk = pd.read_csv(script_path + "\\overleaf_crosswalk.csv")
crosswalk = crosswalk.to_dict(orient='records')

# Iterate through the files in the Downloads folder
for file in crosswalk:
    old_file_path = os.path.join(downloads_path, file['download_name'] + ".pdf")
    
    if file['type'] == "Reading Notes":
        new_file_path = os.path.join(reading_notes_path, file['correct_name'] + ".pdf")
    elif file['type'] == "Course Notes":
        new_file_path = os.path.join(course_notes_path, file['correct_name'] + ".pdf")
        continue

    # Move and rename the file
    #os.rename(old_file_path, new_file_path)
    shutil.move(old_file_path, new_file_path)

    print(f"File '{file}' has been renamed and moved to '{new_file_path}'")
