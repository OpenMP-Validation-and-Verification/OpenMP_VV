#!/usr/bin/env python3

import argparse, glob, sys
import os, json


class testResult:
  ''' 
  Class for storing test results after they are obtained from the log file
  ''' 

  # Attributes

  # Test parameters
  testName = ""
  testPath = ""
  compilerName = ""
  compilerCommand = ""
  testSystem = ""
  testComments = ""

  # Compiler results
  startingCompilerDate = ""
  endingCompilerDate = ""
  compilerPass = ""
  compilerOutput = ""

  # Runtime results
  runtimeOnly = False
  binaryPath = ""
  startingRuntimeDate = ""
  endingRuntimeDate = ""
  runtimePass = ""
  runtimeOutput = ""
  
  def __init__(self):
    # Test parameters
    testName = ""
    testPath = ""
    compilerName = ""
    compilerCommand = ""

    # Compiler results
    startingCompilerDate = ""
    endingCompilerDate = ""
    compilerPass = ""
    compilerOutput = ""

    # Runtime results
    runtimeOnly = False
    binaryPath = ""
    startingRuntimeDate = ""
    endingRuntimeDate = ""
    runtimePass = ""
    runtimeOutput = ""

  def setTestParameters(self, newTestName, newTestPath=None, newCompilerName=None, newCompilerCommand=None):
    self.testName = newTestName
    runtimeOnly = (not newTestPath)
    self.testPath = newTestPath if newTestPath else ""
    self.compilerName = newCompilerName if newCompilerName else ""
    self.compilerCommand = newCompilerCommand if newCompilerCommand else ""

  def setCompilerInit(self, newStartingCompilerDate, newSystem):
    self.startingCompilerDate = newStartingCompilerDate
    self.testSystem = newSystem if newSystem else ""

  def setRuntimeInit(self, newBinaryPath, newStartingRuntimeDate, newSystem):
    self.binaryPath = newBinaryPath
    self.startingRuntimeDate= newStartingRuntimeDate
    self.testSystem = newSystem if newSystem else ""

  def setCompilerResult(self, itPassed, outputText, newEndingCompilerDate, newComments=None):
    ''' setters for compiler results'''
    self.compilerPass = itPassed
    self.compilerOutput = outputText
    self.endingCompilerDate = newEndingCompilerDate
    self.testComments = newComments if newComments else ""

  def setRuntimeResult(self, itPassed, outputText, newEndingRuntimeDate, newComments=None):
    ''' setters for runtime results'''
    self.runtimePass = itPassed
    self.runtimeOutput = outputText
    self.endingRuntimeDate = newEndingRuntimeDate
    self.testComments = newComments if newComments else ""
  
  def makePathRelative(self, basePath=None):
    if (basePath):
      self.testPath = os.path.relpath(self.testPath, basePath)
    else:
      self.testPath = os.path.relpath(self.testPath)

  def convert2CSV(self):
    ''' Comma Separated Values printing '''
    return "%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s" \
           % (self.testName.replace('\n',''), self.testPath.replace('\n',''), self.testSystem.replace('\n',''),
           self.testComments.replace('\n',''), self.compilerName.replace('\n',''),
           self.compilerCommand.replace('\n',''), self.startingCompilerDate.replace('\n',''), 
           self.endingCompilerDate.replace('\n',''), self.compilerPass.replace('\n',''), self.compilerOutput.replace('\n',''),
           str(self.runtimeOnly), self.binaryPath.replace('\n',''), self.startingRuntimeDate.replace('\n',''), 
           self.endingRuntimeDate.replace('\n',''), self.runtimePass.replace('\n',''), self.runtimeOutput.replace('\n',''))

  def convert2dict(self):
    ''' convert to dictionary, easier to jsonify '''
    return {
        "Test name": self.testName, 
        "Test path": self.testPath,
        "Test system": self.testSystem,
        "Test comments": self.testComments,
        "Runtime only": self.runtimeOnly,
        "Compiler name": self.compilerName,
        "Compiler result": self.compilerPass,
        "Compiler output": self.compilerOutput,
        "Compiler command": self.compilerCommand,
        "Compiler starting date": self.startingCompilerDate,
        "Compiler ending date": self.endingRuntimeDate,
        "Binary path": self.binaryPath,
        "Runtime result": self.runtimePass,
        "Runtime output": self.runtimeOutput,
        "Runtime starting date": self.startingRuntimeDate,
        "Runtime ending date": self.endingRuntimeDate
    }
  def __str__(self):
    return """ 
      # Test values
      testName = "%s"
      testPath = "%s"
      testSystem = "%s"
      testComments = "%s"
      compilerName = "%s"
      compilerCommand = "%s"

      # Compiler results
      startingCompilerDate = "%s"
      endingCompilerDate = "%s"
      compilerPass = "%s"
      compilerOutput = "%s"

      # Runtime results
      runtimeOnly = %s
      binaryPath = "%s"
      startingRuntimeDate = "%s"
      endingRuntimeDate = "%s"
      runtimePass = "%s"
      runtimeOutput = "%s"
    """ % (self.testName, self.testPath, self.testSystem, self.testComments, self.compilerName,
           self.compilerCommand, self.startingCompilerDate, 
           self.endingCompilerDate, self.compilerPass, self.compilerOutput,
           str(self.runtimeOnly), self.binaryPath, self.startingRuntimeDate, 
           self.endingRuntimeDate, self.runtimePass, self.runtimeOutput)
  def __repr__(self):
    return str(self) # End of class definition

def parseFile(log_file):
  ''' Function to parsing a single file. given the filename
  it will open the file and obtain all the information, creating an
  array of results
  '''
  returned_value = [];
  #check if log_file is string
  if isinstance(log_file, str):
    #check if log_file is a file that exist
    if os.path.isfile(log_file):
      current_state = "END"
      current_buffer = ""
      current_test = testResult()
      for line in open(log_file,'r'):
        if line.startswith("*-*-*"):
          # header line
          header_info = interpretHeader(line)
          if header_info["type"] == "COMPILE":
            # We are starting a compiler section
            current_test.setTestParameters(header_info["testName"], header_info["file"], header_info["compiler"], header_info["compilerCommand"])
            current_test.setCompilerInit(header_info["date"], header_info["system"])
            current_state = header_info["type"]
          elif header_info["type"] == "RUN":
            # we are starting a runtime section
            if (current_test.testName == ""):
              current_test.setTestParameters(header_info["testName"])
            current_test.setRuntimeInit(header_info["file"], header_info["date"], header_info["system"])
            current_state = header_info["type"]
          elif header_info["type"] == "END":
            # We are ending a section
            if current_state == "COMPILE":
              current_test.setCompilerResult(header_info["result"], current_buffer, header_info["date"], header_info["comments"])
            elif current_state == "RUN":
              current_test.setRuntimeResult(header_info["result"], current_buffer, header_info["date"], header_info["comments"])
              returned_value.append(current_test)
              # Runtime is the last thing that should happen
              current_test = testResult()
              current_state = header_info["type"]

            # reset the values
            current_state = header_info["type"]
            current_buffer = ""
        else:
          # line is just output
          if current_state != "END":
            current_buffer = current_buffer + line

    else: 
      raise ValueError(str(log_file) + " is not a file")
  else: 
    raise ValueError(str(log_file) + " is not a string")
      
  return returned_value
# end of parseFile function definition

def interpretHeader(header):
  ''' Function to split a header into a dictionary containing
  the type of header, and its values'''

  # This is what is returned. It has at least a type and a date
  returned_value = { "type": "", "date": ""}
  if isinstance(header, str):
    header_split = header.split("*-*-*")[1:] # first element always empty
    # get the date 
    returned_value["date"] = header_split[2]
    returned_value["system"] = header_split[3]
    if header_split[0].startswith("BEGIN"):
      if header_split[1].startswith('COMPILE'):
        # case when the header is a compiler header.
        # Example of a compiler line is:
        #   *-*-*COMPILE CC=xlc -I./ompvv -O3 *-*-*path/to/test/test.c*-*-*Thu Jan 18 19:53:25 EST 2018*-*-*

        returned_value["type"] = "COMPILE"
        returned_value["file"] = header_split[4]
        returned_value["testName"] = os.path.basename(header_split[4])
        
        # remove the "COMPILE " part
        compilation_info = header_split[1][8:]
        returned_value["compiler"] = header_split[5]
        if compilation_info[:2] == "CC":
          returned_value["compilerCommand"] = compilation_info[3:]
        elif compilation_info[:3] == "CPP":
          returned_value["compilerCommand"] = compilation_info[4:]
        else:
          returned_value["compilerCommand"] = "undefined"
      elif header_split[1].startswith('RUN'):
        # case when the header is a runtime header.
        # Example of a runtime line is:
        #   *-*-*RUN*-*-*bin/array_segment_map.c*-*-*Thu Jan 18 19:54:32 EST 2018*-*-*
        returned_value["type"] = "RUN"
        returned_value["file"] = header_split[4]
        returned_value["testName"] = os.path.basename(header_split[4])
    elif header_split[0].startswith('END'):
      # case when the header is a closing header.
      # Example of a closing line is:
      #  *-*-*END*-*-*RUN*-*-*Thu Jan 18 19:54:32 EST 2018*-*-*PASS*-*-*
      returned_value["type"] = "END"
      returned_value["result"] = header_split[4]
      returned_value["comments"] = header_split[5]
    else:
      # For some reason, none of the above. Should not happen
      returned_value["type"] = "undefined"
  else:
    raise ValueError("non string sent to parseHeader")

  return returned_value
# end of parseHeader function definition

def main():
  ''' Arguments parsing'''
  parser = argparse.ArgumentParser(description="Process the log files from the SOLLVE OMPVV project")
  parser.add_argument('logFiles', metavar="LOGFILES", type=str, nargs='+', help='the log files to parse')
  parser.add_argument('-f', '--format', dest='format', type=str, nargs=1, default=['JSON'], help='the format to be printed [JSON or CSV]')
  parser.add_argument('-o', '--output', dest='output', type=str, nargs=1, help='output file name')
  parser.add_argument('-r', '--relative-path', dest='relativePath', action='store_true', help="Make paths relative to the path where script is executing")

  args = parser.parse_args()
  
  logFiles = args.logFiles

  results = []

  ''' Action with the arguments '''
  for logfile in logFiles:
    files = glob.iglob(logfile)
    for fileName in files: 
      results.extend(parseFile(fileName))
 
  if len(results) == 0:
    print(" ==> No log files to process")
    return

  if args.relativePath:
    for result in results:
      result.makePathRelative()

  formatedResults = []
  formatedOutput = ""

  if args.format and args.format[0].lower() == 'json':
    for result in results:
      formatedResults.append(result.convert2dict())
    formatedOutput = json.dumps(formatedResults,indent=2, sort_keys=True)
  elif args.format and args.format[0].lower() == 'csv':
    formatedOutput = "testName, testPath, compilerName," \
           "compilerCommand, startingCompilerDate," \
           "endingCompilerDate, compilerPass, compilerOutput," \
           "runtimeOnly, binaryPath, startingRuntimeDate," \
           "endingRuntimeDate, runtimePass, runtimeOutput \n"
    for result in results:
      formatedOutput = formatedOutput + result.convert2CSV() + '\n'

  # Checking if output file was specified
  if args.output and args.output[0]!="":
    outputFile = open(args.output[0], 'w')
    outputFile.write(formatedOutput)
  else:
    print(formatedOutput)
 
# end of main function definition
  
if __name__ == "__main__":
      main()
