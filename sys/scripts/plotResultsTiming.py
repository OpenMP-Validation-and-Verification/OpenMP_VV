import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.cm as cmx
import numpy as np
import re
import os
import argparse, sys


debugMode = 0
includeCuda = False

np.random.seed(1)
linear_colors = [ 'Purples', 'Blues', 'Greens', 'Oranges', 'Reds']
compiler_color_maps = {}

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
  result_omp_runtime = result_total - result_cuda

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
  results[compiler][version][test]["values"].append([result_total, result_cuda, result_omp_runtime])

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
        mean_omp_runtime = np.mean([row[2] for row in values])/1000
        median_omp_runtime = np.median([row[2] for row in values])/1000
        stdev_omp_runtime = np.std([row[2] for row in values])/1000
        results[compiler][version][test]["median"] = [median_total, median_cuda, median_omp_runtime]
        results[compiler][version][test]["mean"] = [mean_total, mean_cuda, mean_omp_runtime]
        results[compiler][version][test]["stdev"] = [stdev_total, stdev_cuda, stdev_omp_runtime]
        printLog(" -> mean = " + str([mean_total, mean_cuda, mean_omp_runtime], ), 3)
        printLog(" -> median = " + str([median_total, median_cuda, median_omp_runtime]), 3)
        printLog(" -> stdev = " + str([stdev_total, stdev_cuda, stdev_omp_runtime]), 3)

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
            if test in results[compiler][version]:
              # Obtaining the values for this test
              newVals["omp_runtime_means"].append(results[compiler][version][test]["mean"][2])
              newVals["omp_runtime_medians"].append(results[compiler][version][test]["median"][2])
              newVals["omp_runtime_stdev"].append(results[compiler][version][test]["stdev"][2])
              newVals["cuda_means"].append(results[compiler][version][test]["mean"][1])
              newVals["cuda_medians"].append(results[compiler][version][test]["median"][1])
              newVals["cuda_stdev"].append(results[compiler][version][test]["stdev"][1])
              
            else:
              # test result does not exist
              newVals["omp_runtime_means"].append(0)
              newVals["omp_runtime_medians"].append(0)
              newVals["omp_runtime_stdev"].append(0)
              newVals["cuda_means"].append(0)
              newVals["cuda_medians"].append(0)
              newVals["cuda_stdev"].append(0)
          values2plot[str(compiler)+" "+str(version)[:20]] = newVals
  bar_width = 0.8/len(values2plot)
  # Adding the plots
  for indx, key in enumerate(values2plot):
    totals = []
    if (includeCuda):
      totals = [values2plot[key]["cuda_means"][i] + values2plot[key]["omp_runtime_means"][i] for i in range(len(values2plot[key]["cuda_means"]))] 
    else:
      totals = values2plot[key]["omp_runtime_means"]
    plt.bar(index + 0.1 + bar_width*indx, totals , bar_width,
    label=key,
    color= values2plot[key]["colorVal"],
    yerr=values2plot[key]["omp_runtime_stdev"])
    if (includeCuda):
      plt.bar(index + 0.1 + bar_width*indx, values2plot[key]["cuda_means"], bar_width,
      color="silver",
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
    plt.savefig(outputFileName+"_target.png")
    createPlot(["target_data_map_to", "target_data_map_from", "target_data_map_tofrom", "target_data_device", "target_data_if"])
    plt.savefig(outputFileName+"_target_data.png")
    createPlot(["target_enter_data_map_to", "target_enter_data_map_alloc", "target_enter_data_map_if_true", "target_enter_data_map_if_false", "target_enter_data_map_device", "target_enter_data_map_depend"])
    plt.savefig(outputFileName+"_target_enter_data.png")
    createPlot(["target_exit_data_map_from", "target_exit_data_map_delete", "target_exit_data_map_if_true", "target_exit_data_map_if_false", "target_exit_data_map_device", "target_exit_data_map_depend"])
    plt.savefig(outputFileName+"_target_exit_data.png")
    createPlot(["target_update_to", "target_update_to_if_true", "target_update_to_if_false", "target_update_to_device", "target_update_to_depend", "target_update_from", "target_update_from_if_true","target_update_from_if_false", "target_update_from_device", "target_update_from_depend"])
    plt.savefig(outputFileName+"_target_update.png")

if __name__ == "__main__":
    main()



