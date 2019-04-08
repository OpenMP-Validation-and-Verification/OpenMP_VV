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
                
            except IOError as e: 
                print("ERROR Opening the file " + fileName)
                print(" Exception = " + str(e))

    data = {'a': np.arange(50),
                'c': np.random.randint(0, 50, 50),
                        'd': np.random.randn(50)}
    data['b'] = data['a'] + 10 * np.random.randn(50)
    data['d'] = np.abs(data['d']) * 100
    
    plt.scatter('a', 'b', c='c', s='d', data=data)
    plt.xlabel('entry a')
    plt.ylabel('entry b')
    plt.savefig(outputFileName)
    
#    # writing the output file
#    with open(outputFileName, "w") as f: 
#        try:
#            printLog("Writing output file " + outputFileName + " containing " + str(result.count("\n")) + " lines")
#            f.write(header)
#            f.write(result)
#        except Exception as e :
#            print ("ERROR writing the file " + outputFileName + " " + str(e))

if __name__ == "__main__":
    main()



