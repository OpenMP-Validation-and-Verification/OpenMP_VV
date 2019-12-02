import abc
import sys
import json
import traceback
import os

### SOLLVE_API_RELATED
class SOLLVE_API_01(metaclass=abc.ABCMeta):
  URL = None
  VER = None
  CREATE_TAG_URL = None
  OBTAIN_RESULT_URL = None
  UPDATE_RESULT_URL = None
  APPEND_RESULT_URL = None
  DELETE_RESULT_URL = None
  DEBUG = -1

  def __init__(self):
    self.print_log("Creating SOLLVE_API_O1", 4)
    self.URL="https://crpl.cis.udel.edu/ompvvsollve/result_report/api/"
    self.VER="0.1"
    self.CREATE_TAG_URL=self.URL+self.VER+'/create_tag'
    self.OBTAIN_RESULT_URL=self.URL+self.VER+'/obtain_result/'
    self.UPDATE_RESULT_URL=self.URL+self.VER+'/update_result/'
    self.APPEND_RESULT_URL=self.URL+self.VER+'/append_result/'
    self.DELETE_RESULT_URL=self.URL+self.VER+'/delete_result/'

  @abc.abstractmethod
  def create_tag(self):
    pass

  @abc.abstractmethod
  def obtain_result(self, tag):
    pass

  @abc.abstractmethod
  def update_result(self, tag, jsonLog):
    pass

  @abc.abstractmethod
  def append_result(self, tag, jsonLog):
    pass

  @abc.abstractmethod
  def delete_result(self,tag):
    pass

  def enable_debug(self, level=1):
    self.DEBUG = level

  def disable_debug(self):
    self.DEBUG = -1

  def print_log(self, msg:str, level=1):
    if (self.DEBUG > level):
      print("=[DEBUG]= "+msg)


## This class if requests is not installed in the system
class SOLLVE_API_01_curl(SOLLVE_API_01):
  def create_tag(self):
    self.print_log("create_tag curl")

  def obtain_result(self, tag):
    self.print_log("obtain_result curl")

  def update_result(self, tag, jsonLog):
    self.print_log("update_result curl")

  def append_result(self, tag, jsonLog):
    self.print_log("append_result curl")

  def delete_result(self,tag):
    self.print_log("delete_result curl")


## This class if request is installed in the system
class SOLLVE_API_01_requests(SOLLVE_API_01):
  def create_tag(self):
    self.print_log("create_tag requests")
    try:
      response = requests.get(self.CREATE_TAG_URL)
      # Connection went well
      if (response.status_code == 200):
        return response.te
      else:
        raise requests.exceptions.HTTPError(f"Status code not as expected {response.status_code}")
    except Exception as err: 
      print(f"Error obtaining tag [{type(err).__name__}]: {err}",file=sys.stderr)


  def obtain_result(self, tag):
    self.print_log("obtain_result requests")
    try:
      response = requests.get(self.OBTAIN_RESULT_URL+tag)
      if (response.status_code == 200):
        return response.json()
      else:
        raise requests.exceptions.HTTPError(f"Status code not as expected {response.status_code}")
    except Exception as err: 
      print(f"Error obtaining tag [{type(err).__name__}]: {err}",file=sys.stderr)

  def update_result(self, tag, jsonLog):
    self.print_log("update_result requests")
    try:
      with open(jsonLog) as f:
        try:
          json_data = json.load(f)
        except Exception as err:
          print(f"Error reading json file {jsonLog}. [{type(err).__name__}] {err}")
        response = requests.post(self.UPDATE_RESULT_URL+tag, headers={'Content-Type': 'application/json'}, json=json_data)
        if (response.status_code == 200):
          if ("false" in response.text):
            return False
          else:
            return True
        else:
          raise requests.exceptions.HTTPError(f"Status code not as expected {response.status_code}")
    except Exception as err: 
      print(f"Error obtaining tag [{type(err).__name__}]: {err}",file=sys.stderr)
      print(traceback.format_exc())
    return False

  def append_result(self, tag, jsonLog):
    self.print_log("append_result requests")
    try:
      with open(jsonLog) as f:
        try:
          json_data = json.load(f)
        except Exception as err:
          print(f"Error reading json file {jsonLog}. [{type(err).__name__}] {err}")
        response = requests.post(self.APPEND_RESULT_URL+tag, headers={'Content-Type': 'application/json'}, json=json_data)
        if (response.status_code == 200):
          if ("false" in response.text):
            return False
          else:
            return True
        else:
          raise requests.exceptions.HTTPError(f"Status code not as expected {response.status_code}")
    except Exception as err: 
      print(f"Error obtaining tag [{type(err).__name__}]: {err}",file=sys.stderr)
      print(traceback.format_exc())
    return False

  def delete_result(self,tag):
    self.print_log("delete_result requests")
    try:
      response = requests.delete(self.DELETE_RESULT_URL+tag)
      if (response.status_code == 200):
        if ("false" in response.text):
          return False
        else:
          return True
      else:
        raise requests.exceptions.HTTPError(f"Status code not as expected {response.status_code}")
    except Exception as err: 
      print(f"Error obtaining tag [{type(err).__name__}]: {err}",file=sys.stderr)

try:
  import requests
  api = SOLLVE_API_01_requests()
except:
  api = SOLLVE_API_01_curl()