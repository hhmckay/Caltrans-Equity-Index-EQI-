# Import necessary packages
import arcpy
import os
import numpy as np

# Define analysis variables
arr = np.array(["500 Meters", "450 meters", "400 Meters", "350 Meters", "300 Meters", "250 Meters", "200 Meters", "150 Meters", "100 Meters", "50 Meters"])
SHN = "USDOT_AADT_07222022"
blocks = "blocks20"

# Define file paths and directory
arcpy.env.workspace = "C:/Users/Username/Downloads/AADT/Data.gdb"
myGDB = "C:/Users/Username/Downloads/AADT/Data.gdb"
out_folder = "C:/Users/Username/Downloads/AADT"

# Overwrites existing output if name is the same
arcpy.env.overwriteOutput = True

# Defines counter variable
Y = 1

# For loop to perform analysis for multiple buffer distances
for X in arr:
    print(Y)
    # Creates buffer to specified radius around SHN lines
    arcpy.analysis.Buffer(SHN, r"memory\SHN_Buffer", X)
    print("{}{}".format("Finished buffer for:", X))

    # Intersect Census blocks with buffers
    arcpy.analysis.PairwiseIntersect([r"memory\SHN_Buffer", blocks], r"memory\Intersected_SHN_Buffers", "ALL", "", "")
    print("{}{}".format("Finished intersect for:", X))
    
    # Add new unique ID field
    arcpy.management.AddField(r"memory\Intersected_SHN_Buffers", "Unique_ID", "TEXT")
    
    # Calculate unique ID
    arcpy.management.CalculateField(r"memory\Intersected_SHN_Buffers", "Unique_ID", "!GEOID20! + '_' + !Route!")
    
    # Find max value per unique ID
    arcpy.Statistics_analysis(r"memory\Intersected_SHN_Buffers", r"memory\Unique_ID_Summary", [["Weighted_AADT_Num", "MAX"]], "Unique_ID")
    
    # Create GEOID
    arcpy.management.AddField(r"memory\Unique_ID_Summary", "GEOID", "TEXT")
    arcpy.management.CalculateField(r"memory\Unique_ID_Summary", "GEOID", "!Unique_ID!.split('_')[0]")
    print("{}{}".format("Finished first calcs for:", X))
    
    # Sum AADT by GEOID
    arcpy.Statistics_analysis(r"memory\Unique_ID_Summary", r"memory\GEOID_Summary", [["MAX_Weighted_AADT_Num", "SUM"]], "GEOID")
    
    # Rename AADT field
    distance = "{}{}".format("AADT", Y)
    arcpy.management.AddField(r"memory\GEOID_Summary", distance, "DOUBLE")
    arcpy.management.CalculateField(r"memory\GEOID_Summary", distance, "!SUM_MAX_Weighted_AADT_Num!")
    arcpy.management.DeleteField(r"memory\GEOID_Summary", ["FREQUENCY", "SUM_MAX_Weighted_AADT_Num"])
    
    # Export table
    out = "{}{}".format("table_", Y)
    arcpy.conversion.TableToTable(r"memory\GEOID_Summary", myGDB, out)
    print("{}{}".format("Finished calcs for:", X))
    arcpy.management.Delete(r"memory")
    Y += 1  
print("finished")

tables = arcpy.ListTables()

for table in tables:
    if table == "table_1":
        pass
    else:
        arcpy.management.JoinField("table_1", "GEOID", table, "GEOID")
        arcpy.management.DeleteField("table_1", "GEOID_1")
        print(table)
print("Finished!")

arcpy.conversion.TableToTable("table_1", out_folder, "AADT_07262022.csv")


tables = arcpy.ListTables()

for table in tables:
    if table == "table_1":
        pass
    else:
        arcpy.management.JoinField("table_1", "GEOID", table, "GEOID")
        arcpy.management.DeleteField("table_1", "GEOID_1")
        print(table)
print("Finished!")
        

arcpy.conversion.TableToTable("table_1", out_folder, "AADT_12062022.csv")


