import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
import re
import os
import argparse, sys


debugMode = 0
def printLog (stringLog, level = 3):
    if (debugMode >= level):
        print ("[DEBUG " + str(level) + "] " + stringLog) 

# Dictionary of compiler -> version -> test -> {values: list([result total, result cuda]), avg...}
results = dict()

def insertInResults(compiler, version, test, result_total, result_cuda):
  # Change to strings just in case
  compiler = str(compiler)
  version = str(version) 
  test = str(test)
  #cleaning results_total and results_cuda
  regex = re.compile("^[0-9]+")
  result_total = regex.match(result_total).group(0)
  result_cuda = regex.match(result_cuda).group(0)

  # Changing to integers
  result_total = int(result_total)
  result_cuda = int(result_cuda)

  # Loging new value
  printLog("Adding new result \n -> compiler = %s \n -> version = %s \n -> test = %s \n -> result_total = %s \n -> result_cuda = %s" % (compiler, version, test, result_total, result_cuda))

  #checking if it exitsts
  if (compiler not in results.keys()):
    printLog("Adding new compiler " + str(compiler))
    printLog("Adding new version " + str(version))
    printLog("Adding new test " + str(test))
    results[compiler] = dict()
    results[compiler][version] = dict()
    results[compiler][version][test] = dict()
    results[compiler][version][test]["values"] = []
  elif (version not in results.get(compiler).keys()):
    printLog("Adding new version " + str(version))
    printLog("Adding new test " + str(test))
    results[compiler][version] = dict()
    results[compiler][version][test] = dict()
    results[compiler][version][test]["values"] = []
  elif (test not in results.get(compiler).get(version).keys()):
    printLog("Adding new test " + str(test))
    results[compiler][version][test] = dict()
    results[compiler][version][test]["values"] = []

  # Inserting the value
  results[compiler][version][test]["values"].append([result_total, result_cuda])

def countResultsStats():
  printLog("There are %d compilers" % (len(results)),1)
  for compiler in results:
    printLog(" -> Compiler " + str(compiler), 1)
    printLog("    There are %d versions" % (len(results[compiler])), 1)
    for version in results[compiler]:
      printLog("    -> Version " + str(version), 1)
      printLog("       There are %d tests" % (len(results[compiler][version])), 1)
      totalValues = 0
      for test in results[compiler][version]:
        totalValues = totalValues + len(results[compiler][version][test]["values"])
      printLog("       There are %d iterations" % (totalValues), 1)

def calculateStats():
  printLog("Calculating stats" ,1)
  for compiler in results:
    printLog(" -> Compiler " + str(compiler), 2)
    for version in results[compiler]:
      printLog("    -> Version " + str(version), 2)
      for test in results[compiler][version]:
        values = results[compiler][version][test]["values"]
        
        mean_total = np.mean([row[0] for row in values])/1000
        median_total = np.median([row[0] for row in values])/1000
        stdev_total = np.std([row[0] for row in values])/1000
        mean_cuda = np.mean([row[1] for row in values])/1000
        median_cuda = np.median([row[1] for row in values])/1000
        stdev_cuda = np.std([row[1] for row in values])/1000
        results[compiler][version][test]["median"] = [median_total, median_cuda]
        results[compiler][version][test]["mean"] = [mean_total, mean_cuda]
        results[compiler][version][test]["stdev"] = [stdev_total, stdev_cuda]
        printLog(" -> mean = " + str([mean_total, mean_cuda]), 3)
        printLog(" -> median = " + str([median_total, median_cuda]), 3)
        printLog(" -> stdev = " + str([stdev_total, stdev_cuda]), 3)

def createPlot(tests, compilers=None, versions=None, labels=None):
  printLog("Plotting ..." ,1)
  if not isinstance(tests, list):
    printLog("ERROR: tests is not a list")
    return None
  # Plot configuration
  n_groups = len(tests)
  opacity = 0.8
  fig, ax = plt.subplots()
  index = np.arange(n_groups)

  # Filtering out the results by test in the tests list and by compilers and versions
  values2plot = {}
  for compiler in results:
    if (compilers is None or compiler in compilers):
      printLog(" -> Compiler " + str(compiler), 2)
      for version in results[compiler]:
        if (versions is None or version in versions):
          printLog("    -> Version " + str(version), 2)
          newVals = {
            "total_means": [],
            "total_medians" : [],
            "total_stdev" : [],
            "cuda_means" : [],
            "cuda_medians" : [],
            "cuda_stdev" : []
          }
          for test in tests:
            if test in results[compiler][version]:
              # Obtaining the values for this test
              newVals["total_means"].append(results[compiler][version][test]["mean"][0])
              newVals["total_medians"].append(results[compiler][version][test]["median"][0])
              newVals["total_stdev"].append(results[compiler][version][test]["stdev"][0])
              newVals["cuda_means"].append(results[compiler][version][test]["mean"][1])
              newVals["cuda_medians"].append(results[compiler][version][test]["median"][1])
              newVals["cuda_stdev"].append(results[compiler][version][test]["stdev"][1])
            else:
              # test result does not exist
              newVals["total_means"].append(0)
              newVals["total_medians"].append(0)
              newVals["total_stdev"].append(0)
              newVals["cuda_means"].append(0)
              newVals["cuda_medians"].append(0)
              newVals["cuda_stdev"].append(0)
          values2plot[str(compiler)+" "+str(version)[:20]] = newVals
  bar_width = 0.8/len(values2plot)
  # Adding the plots
  for indx, key in enumerate(values2plot):
    plt.bar(index + 0.1 + bar_width*indx, values2plot[key]["total_means"], bar_width,
    alpha=opacity,
    label=key,
    yerr=values2plot[key]["total_stdev"])
    plt.bar(index + 0.1 + bar_width*indx, values2plot[key]["cuda_means"], bar_width,
    alpha=opacity,
    color="r",
    label="CUDA" if indx == len(values2plot)-1 else None,
    yerr=values2plot[key]["cuda_stdev"])
  plt.xlabel('clause')
  plt.ylabel('time (us)')
  plt.xticks(index+0.5, tests,  rotation='vertical')
  plt.legend(bbox_to_anchor=(1.05, 1), loc=4, borderaxespad=0.)
  plt.tight_layout()
  

def main():
    ''' Arguments parsing'''
    parser = argparse.ArgumentParser(description="Creates the plots for timings of the different compilers")
    parser.add_argument('fileName', metavar="FILENAME", type=str, nargs=1, help='The file that contains the test summaries (grep logs for SUMMARY)')
    parser.add_argument('--debug', dest='debugMode', type=int, default=0, help='Enable debuging mode')
    parser.add_argument('-o', '--output', dest='output', type=str, nargs=1, required=True, help='output image file name prefix')
    args = parser.parse_args()
   
    
    fileName = args.fileName[0]
    outputFileName = args.output[0]
    if (args.debugMode > 0):
        print ("DEBUG MODE ENABLE")
    global debugMode
    debugMode = args.debugMode

    printLog("Input file name = " + str(args.fileName), 1)
    printLog("Output file name = " + str(args.output), 1)

    # Reading the input file
    if (os.path.exists(fileName)):
        printLog("File " + fileName + " Exists")
        with open(fileName, "r") as f:
            try:
                linesCounter = 0
                for line in f:
                    linesCounter = linesCounter + 1
                    if ( re.search("TEST_SUMMARY", line) ):
                      elements = line.split('\t')
                      insertInResults(elements[1], elements[2], elements[3], elements[4], elements[5])

                printLog("Len of File " + fileName + " = " + str(linesCounter), 1)
                countResultsStats()
                calculateStats()
                
            except IOError as e: 
                print("ERROR Opening the file " + fileName)
                print(" Exception = " + str(e))

    createPlot(["target", "target_defaultmap", "target_dependvar", "target_device", "target_firstprivate", "target_private", "target_if","target_is_device_ptr", "target_map_to", "target_map_from", "target_map_tofrom"])
    plt.savefig(outputFileName)

if __name__ == "__main__":
    main()



