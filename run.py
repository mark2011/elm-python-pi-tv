#!/usr/bin/python
import json, os, subprocess, string, sys, time
import falcon
from wsgiref import simple_server

os.chdir (os.path.dirname (sys.argv [0]))

def standard_response (response, data = []):
    response.append_header ('Access-Control-Allow-Origin', '*')
    response.body = json.dumps (data)

def get_json (request, key):
    s = request.query_string
    s = string.replace (s, "%22", "\"")
    s = string.replace (s, "%20", " ")
    return json.loads (s) [key]

class GetConfigurationResource:
    def on_get (self, request, response):
        config = {}
        try:
           file = open ("/home/pi/.config/elm-python-pi-tv.json", "r")
           try:
              config = json.loads (file.read ())
           except Exception as e:
              print
              print
              print
              print
              print e
           file.close ()
        except Exception as e:
           print
           print
           print
           print
           print
           print e
        standard_response (response, config)

class ListFolderResource:
    def on_get (self, request, response):
        folder = get_json (request, "folder")
        list = [os.path.join (folder, each) for each in os.listdir (folder)]
        list = map (lambda name: [name, os.path.getmtime (name)], list)
        standard_response (response, list)

class LogMessageResource:
    def on_get (self, request, response):
        message = get_json (request, "message")
        print message
        standard_response (response)

class SendKeyResource:
    def on_get (self, request, response):
        key = get_json (request, "key")
        subprocess.call (["xdotool", "search", "--class", "chromium", "key", key])
        standard_response (response)

subprocess.call (["xset", "s", "noblank"])
subprocess.call (["xset", "s", "off"])
subprocess.call (["xset", "-dpms"])
subprocess.Popen (["./process-remote-control-buttons.py"])
subprocess.Popen (["chromium", "elm-python-pi-tv.html"])

api = falcon.API ()
api.add_route (os.path.sep + "log-message", LogMessageResource ())
api.add_route (os.path.sep + "get-configuration", GetConfigurationResource ())
api.add_route (os.path.sep + "list-folder", ListFolderResource ())
api.add_route (os.path.sep + "send-key", SendKeyResource ())
server = simple_server.make_server ("127.0.0.1", 3000, api)
server.serve_forever ()
