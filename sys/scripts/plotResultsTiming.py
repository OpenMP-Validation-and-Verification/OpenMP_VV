import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.cm as cmx
from matplotlib import rc
rc('text', usetex=False)

import numpy as np
import re
import os
import argparse, sys
import json
import math



debugMode = 0
includeCuda = True
histogram_resolution = 500 # in ns
create_histogram = False
currentSystem = "summit"

version_labels={
  "summit": {
    "GCC": "8.1.1",
    "clang": "CORAL 3.8.0",
    "XLC": "16.1.1.0"
  }
}

np.random.seed(1)
linear_colors = [ 'Purples', 'Blues', 'Greens', 'Oranges', 'Reds']
compiler_color_maps = {}
num_teams_colormap = {}

def printLog (stringLog, level = 3):
    if (debugMode >= level):
        print ("[DEBUG " + str(level) + "] " + stringLog) 

# Dictionary of compiler -> version -> test -> {values: list([result total, result cuda]), avg...}
results = dict()

def interpretLine(line, lineCounter):
  result=dict()
  cupti_headers = ["^DRIVER", "^CONC KERNEL", "^RUNTIME", "^KERNEL", "^MEMSET", "^MEMCPY", "^OVERHEAD"]
  if (re.search("^TEST_SUMMARY",line)):
    printLog("Line " + str(lineCounter) + " is TEST_SUMMARY",3)
    # Case where line is a summary, 
    result["line_type"] = "TEST_SUMMARY"
    elements = line.split('\t')
    result["compiler"] = elements[1]
    result["comp_version"] = elements[2]
    result["test_name"] = elements[3]
    # sometimes we need to remove unnecessary information from the times
    regex = re.compile("^[0-9]+")
    result["total_time"] = int(regex.match(elements[4]).group(0))
    result["cuda_time"] = int(regex.match(elements[5]).group(0))
  elif(re.search("^TEST_VARIANT",line)):
    printLog("Line " + str(lineCounter) + " is TEST_VARIANT",3)
    # Case where line is the beginning of a new test type TEST_VARIANT
    result["line_type"] = "NEW_TEST_TYPE"
    elements = line.split('\t')
    if len(elements) == 2:
      # Target teams distribute
      result["test_type"] = "TEST_VARIANT_TTD"
      result["num_teams"] = int(elements[1])
    else:
      result["test_type"] = "TEST_VARIANT_TTDPF"
      result["num_teams"] = int(elements[1])
      result["num_threads"] = int(elements[2])
  elif re.search("^TEST_TIMING_CLAUSES", line) or re.search("^TEST_NESTED", line):
    # Case of timing clauses or nested vs combined
    # for nested vs combined, we must differentiate on plotting
    printLog("Line " + str(lineCounter) + " is " + line,3)
    result["line_type"] = "NEW_TEST_TYPE"
    result["test_type"] = line
  elif (any([re.search(x, line) for x in cupti_headers])):
    printLog("Line " + str(lineCounter) + " is CUPTI_HEADER",3)
    # Case of any of the CUPTI headers 
    result["line_type"] = "CUPTI_HEADER"
    elements = line.split('\t')
    result["cuda_type"] = elements[0]
    result["cuda_values"] = dict()
    result["cuda_values"]["cuda_function"] = elements[1]
    result["cuda_values"]["cuda_start_cycle"] = int(elements[2])
    result["cuda_values"]["cuda_end_cycle"] = int(elements[3])
    result["cuda_values"]["cuda_total_time"] = int(elements[4])
  else:
    result["line_type"] = "IGNORE"
    printLog("Ignoring line " + str(lineCounter),2)
  return result
    
def parseLogFile (fileName):
  printLog("Parsing the file '" + fileName + "'",1)
  curTest=dict()
  with open(fileName, "r") as f:
    try:
        linesCounter = 0
        # For each line, we preprocess it and depending on the type we 
        # fill the curTest information, or we insert the result
        for line in f:
          linesCounter = linesCounter + 1
          line = line.rstrip()
          log_entry = interpretLine(line, linesCounter)
          if (log_entry["line_type"] == "NEW_TEST_TYPE"):
            # Case for new test with a type
            # We clean the whole curTest up since the type will
            # change (it is a different file)
            printLog("Found a new test type %s" % (log_entry["test_type"]),2)
            curTest = dict()

            # getting the test type 
            # (i.e. TEST_VARIANT_TTD, TEST_VARIANT_TTDPF, TEST_TIMING_CLAUSES, and TEST_NESTED)
            # this is later on used for plotting
            curTest["test_type"] = log_entry["test_type"]
            if (log_entry["test_type"] == "TEST_VARIANT_TTD"):
              curTest["num_teams"] = log_entry["num_teams"]
            if (log_entry["test_type"] == "TEST_VARIANT_TTDPF"):
              curTest["num_teams"] = log_entry["num_teams"]
              curTest["num_threads"] = log_entry["num_threads"]

          elif (log_entry["line_type"] == "TEST_SUMMARY"):
            # Case TEST_SUMMARY. A subtest has ended and we need to add it to the database
            curTest["total_time"] = log_entry["total_time"]
            curTest["cuda_time"] = log_entry["cuda_time"]
            curTest["test_name"] = log_entry["test_name"]
            # If the test does not use any cuda, this key will not
            # exist, we need to create it as placeholder
            if ("cuda_values" not in curTest.keys()):
              curTest["cuda_values"] = dict()
            insertInResults(log_entry["compiler"], log_entry["comp_version"], curTest)

            # We need to clean up the subtest information
            del curTest["cuda_values"]
            del curTest["total_time"]
            del curTest["cuda_time"]
            del curTest["test_name"]

          elif (log_entry["line_type"] == "CUPTI_HEADER"):
            # Case where there is information about CUDA
            # We need to store this runtime information
            if ("cuda_values" not in curTest.keys()):
              curTest["cuda_values"] = dict()
            if (log_entry["cuda_type"] not in curTest["cuda_values"]):
              curTest["cuda_values"][log_entry["cuda_type"]] = []
            # This is a list of all the calls for that particular cuda type
            curTest["cuda_values"][log_entry["cuda_type"]].append(log_entry["cuda_values"])
          elif log_entry["line_type"] == "IGNORE":
            continue

        printLog("Len of File " + fileName + " = " + str(linesCounter), 1)
        
    except IOError as e: 
        print("ERROR Opening the file " + fileName)
        print(" Exception = " + str(e))


def insertInResults(compiler, version, test_values):
  # Change to strings just in case
  compiler = str(compiler)
  version = str(version) 

  # Unpack
  test_type = str(test_values["test_type"])
  test_name = str(test_values["test_name"])
  test_total_time = int(test_values["total_time"])
  test_cuda_time = int(test_values["cuda_time"])
  test_values_cuda = test_values["cuda_values"]
  result_omp_runtime = test_total_time - test_cuda_time
  test_num_teams = 0
  test_num_threads = 0
  if (test_type == "TEST_VARIANT_TTD"):
    test_num_teams = test_values["num_teams"] 
  if (test_type == "TEST_VARIANT_TTDPF"):
    test_num_teams = test_values["num_teams"] 
    test_num_threads = test_values["num_threads"] 

  # Loging new value
  printLog("Adding new result \n -> compiler = %s \n -> version = %s \n -> test = %s \n" % (compiler, version, str(test_values)))

  #checking if it exitsts
  if (compiler not in results.keys()):
    printLog("compiler not found")
    printLog("Adding new compiler " + str(compiler))
    printLog("Adding new compiler version " + str(version))
    printLog("Adding new test type " + test_type)
    printLog("Adding new num_teams " + str(test_num_teams))
    printLog("Adding new num_threads " + str(test_num_threads))
    printLog("Adding new subtest name " + test_name)
    results[compiler] = dict()
    results[compiler][version] = dict()
    results[compiler][version][test_type] = dict()
    results[compiler][version][test_type][str(test_num_teams)] = dict()
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)] = dict()
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)][test_name] = dict()
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)][test_name]["values"] = []
  elif (version not in results.get(compiler).keys()):
    printLog("version not found")
    printLog("Adding new compiler version " + str(version))
    printLog("Adding new test type " + test_type)
    printLog("Adding new num_teams " + str(test_num_teams))
    printLog("Adding new num_threads " + str(test_num_threads))
    printLog("Adding new subtest name " + test_name)
    results[compiler][version] = dict()
    results[compiler][version][test_type] = dict()
    results[compiler][version][test_type][str(test_num_teams)] = dict()
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)] = dict()
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)][test_name] = dict()
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)][test_name]["values"] = []
  elif (test_type not in results.get(compiler).get(version).keys()):
    printLog("test_type not found")
    printLog("Adding new test type " + test_type)
    printLog("Adding new num_teams " + str(test_num_teams))
    printLog("Adding new num_threads " + str(test_num_threads))
    printLog("Adding new subtest name " + test_name)
    results[compiler][version][test_type] = dict()
    results[compiler][version][test_type][str(test_num_teams)] = dict()
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)] = dict()
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)][test_name] = dict()
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)][test_name]["values"] = []
  elif (str(test_num_teams) not in results.get(compiler).get(version).get(test_type).keys()):
    printLog("num_teams not found")
    printLog("Adding new num_teams " + str(test_num_teams))
    printLog("Adding new num_threads " + str(test_num_threads))
    printLog("Adding new subtest name " + test_name)
    results[compiler][version][test_type][str(test_num_teams)] = dict()
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)] = dict()
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)][test_name] = dict()
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)][test_name]["values"] = []
  elif (str(test_num_threads) not in results.get(compiler).get(version).get(test_type).get(str(test_num_teams)).keys()):
    printLog("num_threads not found")
    printLog("Adding new num_threads " + str(test_num_threads))
    printLog("Adding new subtest name " + test_name)
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)] = dict()
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)][test_name] = dict()
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)][test_name]["values"] = []
  elif (test_name not in results.get(compiler).get(version).get(test_type).get(str(test_num_teams)).get(str(test_num_threads)).keys()):
    printLog("test_name not found")
    printLog("Adding new subtest name " + test_name)
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)][test_name] = dict()
    results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)][test_name]["values"] = []

  # Inserting the value
  newValue = [test_total_time, test_cuda_time, result_omp_runtime, test_values_cuda]
  results[compiler][version][test_type][str(test_num_teams)][str(test_num_threads)][test_name]["values"].append(newValue)
  

def countResultsStats():
  printLog("There are %d compilers" % (len(results)),1)
  totalResultsCount = 0
  for compiler in results:
    printLog(" -> Compiler " + str(compiler), 1)
    printLog("    There are %d versions" % (len(results[compiler])), 1)
    for version in results[compiler]:
      printLog("    -> Version " + str(version), 1)
      printLog("       There are %d tests types" % (len(results[compiler][version])), 1)
      for test_type in results[compiler][version]:
        totalTestResults = 0
        printLog("      -> Type " + str(test_type), 1)
        printLog("        -> there are %d different teams" % (len(results[compiler][version][test_type])) , 1)
        for num_team in results[compiler][version][test_type]:
          printLog("          -> there are %d different threads" % (len(results[compiler][version][test_type][num_team])) , 1)
          for num_thread in results[compiler][version][test_type][num_team]:
            printLog("            -> there are %d different subtests" % (len(results[compiler][version][test_type][num_team][num_thread])) , 1)
            for test_name in results[compiler][version][test_type][num_team][num_thread]:
              printLog("               -> test %s" % test_name, 2)
              totalTestResults = totalTestResults + len(results[compiler][version][test_type][num_team][num_thread][test_name]["values"])
              printLog("                 -> There are %d iterations" % len(results[compiler][version][test_type][num_team][num_thread][test_name]["values"]), 2)
        printLog("         There are %d iterations" % (totalTestResults), 1)
        totalResultsCount = totalResultsCount + totalTestResults
  printLog("There are %d results in total" % (totalResultsCount),1)
  

# Function to calculate cuda totals regardless of overlapping of cuda calls. 
# it calculates the intersections of the ranges

def calculateFixCudaTotals (values):
  for exp_value in values:
    list_of_starting_points = []
    list_of_end_points = []
    for cuda_type, functions in exp_value[3].items():
      for func in functions: 
        list_of_starting_points.append(int(func["cuda_start_cycle"]))
        list_of_end_points.append(int(func["cuda_end_cycle"]))

    #sorting the arrays
    list_of_starting_points = np.sort(list_of_starting_points)
    list_of_end_points = np.sort(list_of_end_points)

    #both arrays must have the same size (there are always in pairs)
    stack = 0
    i = 0
    j = 0
    previous_mark = 0 # Previously open mark with stack == 0
    total_sum = 0
    while (i != len(list_of_starting_points) and j != len(list_of_end_points)):
      next_element = 0
      if (list_of_starting_points[i] < list_of_end_points[j]): # Found an open
        if (stack == 0): # We start counting
          previous_mark = list_of_starting_points[i]
        # push into stack 
        stack += 1
        # pop the i
        i += 1
      elif (list_of_starting_points[i] >= list_of_end_points[j]): # We found a closing
        if (stack == 1):
          total_sum += list_of_end_points[j] - previous_mark
        stack -= 1
        # pop the j
        j += 1

    #Replace the previously calculated cuda time. which is wrong
    exp_value[1] = total_sum

    #Now we fix the OMP runtime 
    exp_value[2] = exp_value[0] - exp_value[1]

def removeOutliersTotalTime(values):
  max_value = 0
  max_index = -1
  min_value = values[0][0]
  min_index = 0

  for indx, exp_value in enumerate(values):
    if max_value < exp_value[0]: 
      max_value = exp_value[0]
      max_index = indx
    if (min_value > exp_value[0]):
      min_value = exp_value[0]
      min_index = indx

  if (len(values) > 2 ): # if there are only two values leave them alone
    if min_index > max_index:
      values.pop(min_index)
      values.pop(max_index)
    else:
      values.pop(max_index)
      values.pop(min_index)


def calculateStats():
  printLog("Calculating stats" ,1)
  for compiler in results:
    printLog(" -> Compiler " + str(compiler), 2)
    for version in results[compiler]:
      printLog("    -> Version " + str(version), 2)

      for test_type in results[compiler][version]:
        printLog("      -> Type " + str(test_type), 2)
        for num_team in results[compiler][version][test_type]:
          for num_thread in results[compiler][version][test_type][num_team]:
            for test_name in results[compiler][version][test_type][num_team][num_thread]:
              printLog("      -> test name" + str(test_name), 3)
              values = results[compiler][version][test_type][num_team][num_thread][test_name]["values"]
              removeOutliersTotalTime(values)
              # Due to overlapping the cuda time in the logs is wrong. We need to calculate it 
              calculateFixCudaTotals (values) # After calling this row[1] has the real value

              # Calculating aggregated stats 
              # For totals
              listOfTotals = [row[0] for row in values]
              mean_total = np.mean(listOfTotals)
              median_total = np.median(listOfTotals)
              stdev_total = np.std(listOfTotals)
              max_total = int(np.amax(listOfTotals))
              min_total = int(np.amin(listOfTotals))

              # For cuda totals
              listOfCudaTimes = [row[1] for row in values]
              mean_cuda = np.mean(listOfCudaTimes)
              median_cuda = np.median(listOfCudaTimes)
              stdev_cuda = np.std(listOfCudaTimes)
              max_cuda = int(np.amax(listOfCudaTimes))
              min_cuda = int(np.amin(listOfCudaTimes))

              # For OpenMP Runtime 
              listOfRuntimetimes = [row[2] for row in values]
              mean_omp_runtime = np.mean(listOfRuntimetimes)
              median_omp_runtime = np.median(listOfRuntimetimes)
              stdev_omp_runtime = np.std(listOfRuntimetimes)
              max_omp_runtime = int(np.amax(listOfRuntimetimes))
              min_omp_runtime = int(np.amin(listOfRuntimetimes))

              results[compiler][version][test_type][num_team][num_thread][test_name]["median"] = [median_total, median_cuda, median_omp_runtime]
              results[compiler][version][test_type][num_team][num_thread][test_name]["mean"] = [mean_total, mean_cuda, mean_omp_runtime]
              results[compiler][version][test_type][num_team][num_thread][test_name]["stdev"] = [stdev_total, stdev_cuda, stdev_omp_runtime]
              results[compiler][version][test_type][num_team][num_thread][test_name]["max"] = [max_total, max_cuda, max_omp_runtime]
              results[compiler][version][test_type][num_team][num_thread][test_name]["min"] = [min_total, min_cuda, min_omp_runtime]
              printLog(" -> mean = " + str([mean_total, mean_cuda, mean_omp_runtime], ), 3)
              printLog(" -> median = " + str([median_total, median_cuda, median_omp_runtime]), 3)
              printLog(" -> stdev = " + str([stdev_total, stdev_cuda, stdev_omp_runtime]), 3)
              printLog(" -> max = " + str([max_total, max_cuda, max_omp_runtime]), 3)
              printLog(" -> min = " + str([min_total, min_cuda, min_omp_runtime]), 3)

              if (create_histogram):

                # Creating the stats for CUDA world 
                cuda_stats_summary = {}
                number_of_steps_histogram = int(max_total / histogram_resolution)+1
                #Creating the runtime histogram assumed to be all the points where cuda is not running.
                max_histogram_value = len(values)
                results[compiler][version][test_type][num_team][num_thread][test_name]["histogram"] = [max_histogram_value for i in range(number_of_steps_histogram)]

                did_it_decrease = [0 for i in range(number_of_steps_histogram)]

                for indx, row in enumerate(values): #values is an array of experimental results
                  for i in range(number_of_steps_histogram):
                    did_it_decrease[i] = 0
                    
                  for cuda_type, cuda_events_list in row[3].items(): # e.g. DRIVER, MEMCPY, MEMSET... #cuda event is the array of events
                    # First experiment, let's create the summary structure for this type 
                    if (cuda_type not in cuda_stats_summary.keys()):
                      cuda_stats_summary[cuda_type] = {}
                      printLog(" sumarizing cuda type " + cuda_type + " for test " + test_name)
                      cuda_stats_summary[cuda_type]["mean_duration"] = []
                      # prefill the events array with the first occurrance
                      cuda_stats_summary[cuda_type]["histogram"] = {}

                    # Calculating the total execution time of the event
                    cuda_stats_summary[cuda_type]["mean_duration"].append( np.sum([ event["cuda_total_time"] for event in cuda_events_list ]))

                    for event in cuda_events_list:
                      if (event["cuda_function"] not in cuda_stats_summary[cuda_type]["histogram"].keys()): 
                        cuda_stats_summary[cuda_type]["histogram"][event["cuda_function"]] = [0 for x in range(number_of_steps_histogram)]

                      # Get lower limit in the histogram
                      lower_limit = math.ceil(event["cuda_start_cycle"]/histogram_resolution)
                      upper_limit = int(event["cuda_end_cycle"]/histogram_resolution)

                      for i in range(lower_limit, upper_limit+1):
                        cuda_stats_summary[cuda_type]["histogram"][event["cuda_function"]][i] += 1
                        # Wherever cuda is happenning, omp_runtime is not
                        if (did_it_decrease[i] == 0 ):
                          did_it_decrease[i] = 1
                          results[compiler][version][test_type][num_team][num_thread][test_name]["histogram"][i] -= 1
                    
                  # Remove the trailing execution after the final execution time
                  # Since the histogram is as long as the max exec time, we want to 
                  # see the exec time distribution as well
                  lower_limit = int(row[0]/histogram_resolution)
                  for i in range(lower_limit, number_of_steps_histogram):
                    results[compiler][version][test_type][num_team][num_thread][test_name]["histogram"][i] -= 1



                # So far we have created a list of durations, now we need to create the statistic values
                for cuda_type in cuda_stats_summary:
                  array_of_durations = cuda_stats_summary[cuda_type]["mean_duration"]
                  cuda_stats_summary[cuda_type]["median_duration"] = np.median(array_of_durations)
                  cuda_stats_summary[cuda_type]["stdev_duration"] = np.std(array_of_durations)
                  cuda_stats_summary[cuda_type]["mean_duration"] = np.mean(array_of_durations)
                # Let's add the cuda summary to the results
                results[compiler][version][test_type][num_team][num_thread][test_name]["cuda_stats"] = cuda_stats_summary

                # we create a histogram for each of the cuda events
    

def createPlotTimelineClause(test, test_type, compilers=None, versions=None, labels=None):
  if not create_histogram:
    printLog("Histogram creation is dissabled. Enabel it first to allow plot timeline", 0)
    return None
  printLog("Plotting TEST_CLAUSE timeline..." ,1)
  if not isinstance(test, str):
    printLog("ERROR: tests is a string")
    return None

  # Calculating number of subplots
  num_cuda_functions_per_compiler = []
  num_compilers = 0
  max_x_value = 0
  for compiler in results:
    if (compilers is None or compiler in compilers):
      num_compilers += 1
      for version in results[compiler]:
        if (versions is None or version in versions):
          for cuda_type, cuda_stat in results[compiler][version][test_type]["0"]["0"][test]["cuda_stats"].items():
            num_cuda_functions_per_compiler.append(len(cuda_stat['histogram']))
            #Obtaining the largest histogram x time to scale the other ones to that one
            for cuda_type, cuda_hist in cuda_stat["histogram"].items():
              if len(cuda_hist) > max_x_value:
                max_x_value = len(cuda_hist)
                

  printLog ("Plotting %d compilers with a total of %d subgraphs" % (num_compilers, np.sum(num_cuda_functions_per_compiler) + 1),2)
  num_subplots = int(num_compilers*2 - 1 + np.sum(num_cuda_functions_per_compiler))
  fig, axes = plt.subplots(nrows=num_subplots+1, sharex='col', figsize=(9, 10)) # The +1 is necessary to plot the last imshow. I don't know why
  fig.subplots_adjust(left=0.25, hspace=0)
  cur_ax = 0

  #White space images
  white_space_cnt = 0

  # Filtering out the results by test in the tests list and by compilers and versions
  for compiler in results:
    if (compilers is None or compiler in compilers):
      if (white_space_cnt != 0):
        axes[cur_ax].axis("off")
        cur_ax += 1
      white_space_cnt += 1
      printLog(" -> Compiler " + str(compiler), 2)

      for version in results[compiler]:
        if (versions is None or version in versions):
          printLog("    -> Version " + str(version), 2)

          #TODO: Change the num_threads and num_teams variable somehow
          if (test in results[compiler][version][test_type]["0"]["0"].keys()):
            printLog("      -> Found a test ", 2)
            axes[cur_ax].set_yticks([],minor=False)
            # Printing runtime histogram
            runtime_hist = results[compiler][version][test_type]["0"]["0"][test]["histogram"]
            runtime_hist += [0 for i in range(max_x_value - len(runtime_hist))]
            hist = np.vstack((runtime_hist, runtime_hist))

            # Plot the rintime timeline 
            num_values = len(results[compiler][version][test_type]["0"]["0"][test]["values"])
            axes[cur_ax].imshow(hist, aspect='auto', cmap="RdBu", vmin = -1*num_values, vmax = num_values, extent=[0, len(hist)*histogram_resolution, 0, 1])
            pos = list(axes[cur_ax].get_position().bounds)
            x_text = pos[0] - 0.01
            y_text = pos[1] + pos[3]/2
            fig.text(x_text, y_text, "OMP Runtime", va='center', ha='right', fontsize=10)
            axes[cur_ax].set_title("%s %s" % (compiler, version[0:(50 if len(version) > 50 else len(version) - 1)]))
            cur_ax += 1

            # Plotting the Cuda functions
            for cuda_type, cuda_stat in results[compiler][version][test_type]["0"]["0"][test]["cuda_stats"].items():
              for cuda_function, cuda_hist in cuda_stat["histogram"].items():
                hist = cuda_hist + [0 for i in range(max_x_value - len(cuda_hist))]
                hist = np.vstack((hist,hist))
                axes[cur_ax].set_yticks([],minor=False)
                print("'cuda_fucn' = %s cuda_hist = %d, Max_x_val = %d" % (cuda_function, len(cuda_hist), max_x_value))
                axes[cur_ax].imshow(hist, aspect='auto', cmap="PiYG", vmin = -1*num_values, vmax = num_values, extent=[0, len(hist)*histogram_resolution, 0, 1])
                pos = list(axes[cur_ax].get_position().bounds)
                x_text = pos[0] - 0.01
                y_text = pos[1] + pos[3]/2.
                fig.text(x_text, y_text, cuda_function, va='center', ha='right', fontsize=10)
                cur_ax +=1
  
  # For some reason this is necessary to show the last plot
  axes[cur_ax].set_yticks([],minor=False)
  axes[cur_ax].spines['top'].set_visible(False)
  axes[cur_ax].spines['right'].set_visible(False)
  axes[cur_ax].spines['left'].set_visible(False)
  axes[cur_ax].set_xticks([ int(i*max_x_value*histogram_resolution/5) for i in range(5)], minor=False)


            
def createPlotTimingClauses(tests, compilers=None, versions=None, labels=None):
  printLog("Plotting TEST_TIMING_CLAUSES..." ,1)
  if not isinstance(tests, list):
    printLog("ERROR: tests is not a list")
    return None
  # Plot configuration
  n_groups = len(tests)
  opacity = 0.8
  fig, ax = plt.subplots(figsize=(10, 9))
  index = np.arange(n_groups)

  # Filtering out the results by test in the tests list and by compilers and versions
  values2plot = {}
  for compiler in results:
    if (compilers is None or compiler in compilers):
      # For choosing the color map
      scalarMap = []
      if (compiler not in compiler_color_maps.keys()):
        selectColorMap = np.random.randint(0,len(linear_colors)-1)
        cm = plt.get_cmap(linear_colors[selectColorMap])
        cNorm  = colors.Normalize(vmin=-len(results[compiler]), vmax=len(results[compiler])*2)
        scalarMap = cmx.ScalarMappable(norm=cNorm, cmap=cm)
        compiler_color_maps[compiler] = scalarMap
      else :
        scalarMap = compiler_color_maps[compiler]
      nextColor = len(results[compiler])

      printLog(" -> Compiler " + str(compiler), 2)
      for version in results[compiler]:
        if (versions is None or version in versions):
          colorVal = scalarMap.to_rgba(nextColor)
          nextColor = nextColor - 1
          printLog("    -> Version " + str(version), 2)
          newVals = {
            "omp_runtime_means": [],
            "omp_runtime_medians" : [],
            "omp_runtime_stdev" : [],
            "cuda_means" : [],
            "cuda_medians" : [],
            "cuda_stdev" : [],
            "colorVal" : colorVal
          }
          for test in tests:
            test_results = results[compiler][version]["TEST_TIMING_CLAUSES"]['0']['0']
            if test in test_results:
              # Obtaining the values for this test
              newVals["omp_runtime_means"].append(test_results[test]["mean"][2])
              newVals["omp_runtime_medians"].append(test_results[test]["median"][2])
              newVals["omp_runtime_stdev"].append(test_results[test]["stdev"][2])
              newVals["cuda_means"].append(test_results[test]["mean"][1])
              newVals["cuda_medians"].append(test_results[test]["median"][1])
              newVals["cuda_stdev"].append(test_results[test]["stdev"][1])
              
            else:
              # test result does not exist
              newVals["omp_runtime_means"].append(0)
              newVals["omp_runtime_medians"].append(0)
              newVals["omp_runtime_stdev"].append(0)
              newVals["cuda_means"].append(0)
              newVals["cuda_medians"].append(0)
              newVals["cuda_stdev"].append(0)
          plot_label = str(compiler)+" "+version_labels[currentSystem][compiler]
          values2plot[plot_label] = newVals
  bar_width = 0.8/len(values2plot)
  # Adding the plots
  for indx, key in enumerate(values2plot):
    totals = []
    if (includeCuda):
      totals = [values2plot[key]["cuda_medians"][i] + values2plot[key]["omp_runtime_medians"][i] for i in range(len(values2plot[key]["cuda_medians"]))] 
    else:
      totals = values2plot[key]["omp_runtime_medians"]
    totals = [i/1000 for i in totals]
    # plt.bar(index + 0.1 + bar_width*indx, totals , bar_width,
    # label=key,
    # color= values2plot[key]["colorVal"],
    # yerr=values2plot[key]["omp_runtime_stdev"])
    plt.bar(index + 0.1 + bar_width*indx, totals , bar_width,
    label=key,
    color= values2plot[key]["colorVal"])
    if (includeCuda):
      # plt.bar(index + 0.1 + bar_width*indx, values2plot[key]["cuda_medians"], bar_width,
      # color="silver",
      # label="CUDA" if indx == len(values2plot)-1 else None,
      # yerr=values2plot[key]["cuda_stdev"])
      cuda_values = [i/1000 for i in values2plot[key]["cuda_medians"]]
      plt.bar(index + 0.1 + bar_width*indx,cuda_values, bar_width,
      color="silver",
      label="CUDA" if indx == len(values2plot)-1 else None)
  plt.xlabel('clause', fontsize = 18)
  plt.ylabel('time (us)', fontsize = 18)
  plt.xticks(index + 0.35, (tests if labels is None else labels),  rotation="vertical", fontsize=12)
  plt.legend(loc=0, fontsize=18)
  plt.tight_layout()
  
def createPlotNumTeams(tests, compilers=None, versions=None, labels=None):
  printLog("Plotting TEST_VARIANT_TTD..." ,1)
  if not isinstance(tests, list):
    printLog("ERROR: tests is not a list", 0)
    return None
  if versions is None:
    versions = {}
    for compiler in results:
      if (compilers is None or compiler in compilers):
        versions[compiler] = [i for i in results[compiler].keys()][0] # Use the first version available
        printLog("WARNING: No version specified. Using " + versions[compiler], 0)

  # Plot configuration
  n_groups = len(tests)
  index = np.arange(n_groups)

  num_subplots = len(compilers) if compilers is not None else len(results)
  fig, axes = plt.subplots(num_subplots,sharex='col', figsize=(9, 10))
  if type(axes) is not np.ndarray:
    axes = [axes]
  cur_ax = 0
  selectColorMap = 0
  # Filtering out the results by test in the tests list and by compilers and versions
  for compiler in results:
    if (compilers is None or compiler in compilers):
      printLog(" -> Compiler " + str(compiler), 2)
      version = versions[compiler]
      values2plot = {}
      for num_team in results[compiler][version]["TEST_VARIANT_TTD"]:
        # For choosing the color map
        scalarMap = []
        cm = ""
        if (num_team not in num_teams_colormap.keys()):
          cm = plt.get_cmap(linear_colors[selectColorMap])
          selectColorMap += 1 
          cNorm  = colors.Normalize(vmin=0, vmax=1)
          scalarMap = cmx.ScalarMappable(norm=cNorm, cmap=cm)
          num_teams_colormap[num_team] = scalarMap
        else :
          scalarMap = num_teams_colormap[num_team]
        printLog("    -> Version " + version, 2)
        newVals = {
          "omp_runtime_means": [],
          "omp_runtime_medians" : [],
          "omp_runtime_stdev" : [],
          "cuda_means" : [],
          "cuda_medians" : [],
          "cuda_stdev" : [],
          "cm" : scalarMap.to_rgba(0.75)
        }
        for test in tests:
          test_results = results[compiler][version]["TEST_VARIANT_TTD"][num_team]['0']
          if test in test_results:
            # Obtaining the values for this test
            newVals["omp_runtime_means"].append(test_results[test]["mean"][2])
            newVals["omp_runtime_medians"].append(test_results[test]["median"][2])
            newVals["omp_runtime_stdev"].append(test_results[test]["stdev"][2])
            newVals["cuda_means"].append(test_results[test]["mean"][1])
            newVals["cuda_medians"].append(test_results[test]["median"][1])
            newVals["cuda_stdev"].append(test_results[test]["stdev"][1])
            
          else:
            # test result does not exist
            newVals["omp_runtime_means"].append(0)
            newVals["omp_runtime_medians"].append(0)
            newVals["omp_runtime_stdev"].append(0)
            newVals["cuda_means"].append(0)
            newVals["cuda_medians"].append(0)
            newVals["cuda_stdev"].append(0)
        values2plot[num_team] = newVals
      bar_width = 0.8/len(values2plot)
      # Adding the plots
      for indx, key in enumerate(values2plot):
        totals = []
        if (includeCuda):
          totals = [values2plot[key]["cuda_medians"][i] + values2plot[key]["omp_runtime_medians"][i] for i in range(len(values2plot[key]["cuda_medians"]))] 
        else:
          totals = values2plot[key]["omp_runtime_medians"]
        axes[cur_ax].bar(index + 0.1 + bar_width*indx, totals , bar_width,
        label=key + " teams",
        color = values2plot[key]["cm"])
        if (includeCuda):
          axes[cur_ax].bar(index + 0.1 + bar_width*indx, values2plot[key]["cuda_medians"], bar_width,
          color = "silver",
          alpha =0.91,
          label="CUDA" if indx == len(values2plot)-1 else None)
      if (cur_ax == 0):
        axes[cur_ax].legend()
      if (cur_ax == num_subplots - 1):
        axes[cur_ax].set(xlabel = 'clause')
        
        axes[cur_ax].set_xticks(index+0.5)
        axes[cur_ax].set_xticklabels(tests if labels is None else labels, rotation='vertical')
      axes[cur_ax].set(ylabel =' time (us)')
      cur_ax += 1
  plt.tight_layout()
  cur_ax = 0
  for compiler in results:
    if (compilers is None or compiler in compilers):
      pos = list(axes[cur_ax].get_position().bounds)
      x_text = pos[0] + 0.01
      y_text = pos[1] + pos[3] - 0.01
      fig.text(x_text, y_text, compiler+ " " + version_labels[currentSystem][compiler],  va='center', fontsize=10)
      cur_ax += 1


def createPlotNumTeamsNumThreads(test, compilers=None, versions=None):
  printLog("Plotting TEST_VARIANT_TTDPF..." ,1)
  if not isinstance(test, str):
    printLog("ERROR: tests is not a string", 0)
    return None
  if (versions is not None) and not isinstance(versions, list):
    printLog("ERROR: versions need to be a string", 0)
    return None

  # Plot configuration
  num_of_compilers = len(compilers) if compilers is not None else len(results)
  fig, axes = plt.subplots(ncols=num_of_compilers, sharex='row', figsize=(10*num_of_compilers, 9))
  if type(axes) is not np.ndarray:
    axes = [axes]
  cur_ax = 0

  # Filtering out the results by test in the tests list and by compilers and versions
  for compiler in results:
    if (compilers is None or compiler in compilers):
      printLog(" -> Compiler " + str(compiler), 2)
      num_of_versions = len(versions) if versions is not None else len(results[compiler])
      values2plot = {}
      current_version_index = 0
      for version in results[compiler]:
        if (versions is None or version in versions):
          printLog("    -> Version " + version, 2)
          for num_team in results[compiler][version]["TEST_VARIANT_TTDPF"]:
            if num_team not in values2plot.keys():
              values2plot[num_team] = {}
            for num_threads in results[compiler][version]["TEST_VARIANT_TTDPF"][num_team]:
              if num_threads not in values2plot[num_team].keys():
                values2plot[num_team][num_threads] =  {
                                                        "omp_runtime_means": [0 for i in range(num_of_versions)],
                                                        "omp_runtime_medians" : [0 for i in range(num_of_versions)],
                                                        "omp_runtime_stdev" : [0 for i in range(num_of_versions)],
                                                        "cuda_means" : [0 for i in range(num_of_versions)],
                                                        "cuda_medians" : [0 for i in range(num_of_versions)],
                                                        "cuda_stdev" : [0 for i in range(num_of_versions)]
                                                        }

              test_results = results[compiler][version]["TEST_VARIANT_TTDPF"][num_team][num_threads]
              if test in test_results:
                # Obtaining the values for this test
                values2plot[num_team][num_threads]["omp_runtime_means"][current_version_index] = test_results[test]["mean"][2]
                values2plot[num_team][num_threads]["omp_runtime_medians"][current_version_index] = test_results[test]["median"][2]
                values2plot[num_team][num_threads]["omp_runtime_stdev"][current_version_index] = test_results[test]["stdev"][2]
                values2plot[num_team][num_threads]["cuda_means"][current_version_index] = test_results[test]["mean"][1]
                values2plot[num_team][num_threads]["cuda_medians"][current_version_index] = test_results[test]["median"][1]
                values2plot[num_team][num_threads]["cuda_stdev"][current_version_index] = test_results[test]["stdev"][1]
                
              else:
                # test result does not exist
                values2plot[num_team][num_threads]["omp_runtime_means"][current_version_index] = 0
                values2plot[num_team][num_threads]["omp_runtime_medians"][current_version_index] = 0
                values2plot[num_team][num_threads]["omp_runtime_stdev"][current_version_index] = 0
                values2plot[num_team][num_threads]["cuda_means"][current_version_index] = 0
                values2plot[num_team][num_threads]["cuda_medians"][current_version_index] = 0
                values2plot[num_team][num_threads]["cuda_stdev"][current_version_index] = 0
      bar_width = 0.8/num_of_versions
      # Adding the plots
      current_plot_group = 0
      labels = []
      ticks = []
      for indx_teams, num_team in enumerate(values2plot):
        totals = []
        totals_cuda = []
        indexes = []
        for indx_threads, num_threads in enumerate(values2plot[num_team]):
          if (includeCuda):
            totals += [values2plot[num_team][num_threads]["cuda_medians"][i] + values2plot[num_team][num_threads]["omp_runtime_medians"][i] for i in range(num_of_versions)]
            totals_cuda += values2plot[num_team][num_threads]["cuda_medians"]
          else:
            totals += [values2plot[num_team][num_threads]["omp_runtime_medians"][i] for i in range(num_of_versions)]
          indexes += [current_plot_group + 0.1 + bar_width*i for i in range(num_of_versions)]
          labels.append(num_threads)
          ticks += [current_plot_group+.1]
          current_plot_group += 1
        # if (cur_ax == 0):
        # if (cur_ax == num_subplots - 1):
        #   axes[cur_ax].set(xlabel = 'clause')
        totals= [i/1000 for i in totals]
        totals_cuda= [i/1000 for i in totals_cuda]
        axes[cur_ax].bar(indexes , totals , bar_width, label= num_team + " teams", edgecolor= "black")
        if (includeCuda):
          axes[cur_ax].bar(indexes, totals_cuda, bar_width,
          color = "silver",
          alpha =0.91,
          label="CUDA" if indx_teams == len(values2plot)-1 else None)
        current_plot_group += 1
      #Adding plot info
      axes[cur_ax].legend()
      axes[cur_ax].set_xticks(ticks)
      axes[cur_ax].set_xticklabels(labels)
      axes[cur_ax].set(ylabel ='NumTeams Num Threads')
      cur_ax += 1
  plt.tight_layout()
  cur_ax = 0
  for compiler in results:
    if (compilers is None or compiler in compilers):
      pos = list(axes[cur_ax].get_position().bounds)
      x_text = pos[0] + 0.01
      y_text = pos[1] + pos[3] - 0.01
      fig.text(x_text, y_text, compiler+ " " + version_labels[currentSystem][compiler],  va='center', fontsize=10)
      cur_ax += 1


def main():
    global results
    ''' Arguments parsing'''
    parser = argparse.ArgumentParser(description="Creates the plots for timings of the different compilers")
    parser.add_argument('fileName', metavar="FILENAME", type=str, nargs=1, help='The file that contains the test summaries (grep logs for SUMMARY)')
    parser.add_argument('--debug', dest='debugMode', type=int, default=0, help='Enable debuging mode')
    parser.add_argument('-o', '--output', dest='output', type=str, nargs=1, required=True, help='output image file name prefix')
    parser.add_argument('-c', '--cache', dest='cacheFile', type=str, nargs=1, default="", help='output cache file name prefix')
    args = parser.parse_args()
   
    
    fileName = args.fileName[0]
    outputFileName = args.output[0]
    cacheFileName = args.cacheFile[0]
    if (args.debugMode > 0):
        print ("DEBUG MODE ENABLE")
    global debugMode
    debugMode = args.debugMode

    printLog("Input file name = " + str(args.fileName), 1)
    printLog("Output file name = " + str(args.output), 1)
    printLog("Cache file name = " + str(args.cacheFile), 1)

    # Reading the input file
    if (os.path.exists(fileName)):
        printLog("File " + fileName + " Exists")
        if (len(cacheFileName) != 0 and os.path.exists(cacheFileName)):
          printLog("Reading results from cachefile " + cacheFileName, 1)
          with open(cacheFileName, "r") as cacheFile:
            results = json.load(cacheFile)
        else:
          parseLogFile(fileName)
          if (len(cacheFileName) != 0):
            printLog("saving results into cachefile " + cacheFileName, 1)
            with open(cacheFileName, "w") as cacheFile:
              json.dump(results, cacheFile , indent=2)
    countResultsStats()
    calculateStats()

    # Target
    labels = ["target", "defaultmap", "dependvar", "device", "firstprivate", "private", "if","is_device_ptr", "map_to", "map_from", "map_tofrom"]
    tests_to_plot = ["target", "target_defaultmap", "target_dependvar", "target_device", "target_firstprivate", "target_private", "target_if","target_is_device_ptr", "target_map_to", "target_map_from", "target_map_tofrom"]
    createPlotTimingClauses(tests_to_plot, labels = labels)
    plt.savefig(outputFileName+"_target.png")
    # Target Data
    labels = ["map_to", "map_from", "map_tofrom", "device", "if"]
    tests_to_plot = ["target_data_map_to", "target_data_map_from", "target_data_map_tofrom", "target_data_device", "target_data_if"]
    createPlotTimingClauses(tests_to_plot, labels = labels)
    plt.savefig(outputFileName+"_target_data.png")
    # Target enter data
    labels = ["map_to", "map_alloc", "map_if_true", "map_if_false", "map_device", "map_depend"]
    tests_to_plot = ["target_enter_data_map_to", "target_enter_data_map_alloc", "target_enter_data_map_if_true", "target_enter_data_map_if_false", "target_enter_data_map_device", "target_enter_data_map_depend"]
    createPlotTimingClauses(tests_to_plot, labels = labels)
    plt.savefig(outputFileName+"_target_enter_data.png")
    # Target exit data
    labels = ["map_from", "map_delete", "map_if_true", "map_if_false", "map_device", "map_depend"]
    tests_to_plot = ["target_exit_data_map_from", "target_exit_data_map_delete", "target_exit_data_map_if_true", "target_exit_data_map_if_false", "target_exit_data_map_device", "target_exit_data_map_depend"]
    createPlotTimingClauses(tests_to_plot, labels = labels)
    plt.savefig(outputFileName+"_target_exit_data.png")
    # Target update
    labels = ["to", "to_if_true", "to_if_false", "to_device", "to_depend", "from", "from_if_true", "from_if_false", "from_device", "from_depend"]
    tests_to_plot = ["target_update_to", "target_update_to_if_true", "target_update_to_if_false", "target_update_to_device", "target_update_to_depend", "target_update_from", "target_update_from_if_true","target_update_from_if_false", "target_update_from_device", "target_update_from_depend"]
    createPlotTimingClauses(tests_to_plot, labels = labels)
    plt.savefig(outputFileName+"_target_update.png")
    # for i in tests_to_plot:
    #   createPlotTimelineClause(i,"TEST_TIMING_CLAUSES", ["GCC","XLC","clang"])
    #   plt.savefig(outputFileName+i+"_timeline.png")


    labels = ["(NO CLAUSE)",
              "collapse",
              "defaultmap",
              "default(none)",
              "default(shared)",
              "depend",
              "device",
              "firstprivate",
              "if(true)",
              "if(false)",
              "lastprivate",
              "map(to)",
              "map(from)",
              "map(tofrom)",
              "map(alloc)",
              "private",
              "shared"]
            
    tests_to_plot = ["target teams distribute parallel for",
                    "target teams distribute parallel for collapse",
                    "target teams distribute parallel for defaultmap",
                    "target teams distribute parallel for default(none)",
                    "target teams distribute parallel for default(shared)",
                    "target teams distribute parallel for depend",
                    "target teams distribute parallel for device",
                    "target teams distribute parallel for firstprivate",
                    "target teams distribute parallel for if(true)",
                    "target teams distribute parallel for if(false)",
                    "target teams distribute parallel for lastprivate",
                    "target teams distribute parallel for map(to)",
                    "target teams distribute parallel for map(from)",
                    "target teams distribute parallel for map(tofrom)",
                    "target teams distribute parallel for map(alloc)",
                    "target teams distribute parallel for private",
                    "target teams distribute parallel for shared"]

    createPlotTimingClauses(tests_to_plot, labels=labels)
    plt.savefig(outputFileName+"_target_teams_distribute_parallel_for.png")
    tests_to_plot = ["target teams distribute",
                    "target teams distribute collapse",
                    "target teams distribute defaultmap",
                    "target teams distribute default(none)",
                    "target teams distribute default(shared)",
                    "target teams distribute depend",
                    "target teams distribute device",
                    "target teams distribute firstprivate",
                    "target teams distribute if(true)",
                    "target teams distribute if(false)",
                    "target teams distribute lastprivate",
                    "target teams distribute map(to)",
                    "target teams distribute map(from)",
                    "target teams distribute map(tofrom)",
                    "target teams distribute map(alloc)",
                    "target teams distribute private",
                    "target teams distribute shared"]
    createPlotTimingClauses(tests_to_plot, labels=labels)
    plt.savefig(outputFileName+"_target_teams_distribute.png")


    labels = ["(NO CLAUSE)",
              "collapse",
              "defaultmap",
              "default(none)",
              "default(shared)",
              "depend",
              "device",
              "firstprivate",
              "if(true)",
              "lastprivate",
              "map(to)",
              "map(from)",
              "map(tofrom)",
              "map(alloc)",
              "private",
              "shared"]
    tests_to_plot = ["target teams distribute",
                    "target teams distribute collapse",
                    "target teams distribute defaultmap",
                    "target teams distribute default(none)",
                    "target teams distribute default(shared)",
                    "target teams distribute depend",
                    "target teams distribute device",
                    "target teams distribute firstprivate",
                    "target teams distribute if(true)",
                    "target teams distribute lastprivate",
                    "target teams distribute map(to)",
                    "target teams distribute map(from)",
                    "target teams distribute map(tofrom)",
                    "target teams distribute map(alloc)",
                    "target teams distribute private",
                    "target teams distribute shared"]
    createPlotNumTeams(tests_to_plot, ["clang", "XLC", "GCC"], labels=labels)
    plt.savefig(outputFileName+"_target_teams_distribute_num_teams.png")

    #Timelines! 

    # Target
    # createPlotTimelineClause("target","TEST_TIMING_CLAUSES", ["GCC","XLC","clang"])
    # plt.savefig(outputFileName+"target_timeline.png")
    # createPlotTimelineClause("target_data_map_to","TEST_TIMING_CLAUSES", ["GCC","XLC","clang"])
    # plt.savefig(outputFileName+"target_data_map_to_timeline.png")
    # createPlotTimelineClause("target_enter_data_map_to","TEST_TIMING_CLAUSES", ["GCC","XLC","clang"])
    # plt.savefig(outputFileName+"target_enter_data_map_to_timeline.png")
    # createPlotTimelineClause("target_exit_data_map_from","TEST_TIMING_CLAUSES", ["GCC","XLC","clang"])
    # plt.savefig(outputFileName+"target_exit_data_map_from_timeline.png")
    # createPlotTimelineClause("target teams distribute","TEST_TIMING_CLAUSES", ["GCC","XLC","clang"])
    # plt.savefig(outputFileName+"target_teams_distribute.png")
    # createPlotTimelineClause("target teams distribute parallel for","TEST_TIMING_CLAUSES", ["GCC","XLC","clang"])
    # plt.savefig(outputFileName+"target_Teams_distribute_parallel_for_timeline.png")


    createPlotNumTeamsNumThreads("target teams distribute parallel for", ["GCC", "XLC", "clang"])
    plt.savefig(outputFileName+"_target_teams_distribute_parallel_for_num_teams_num_threads.png")


if __name__ == "__main__":
    main()



