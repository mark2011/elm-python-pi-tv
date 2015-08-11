#!/usr/bin/python
import json, os, subprocess, sys, time
import falcon
from wsgiref import simple_server

os.chdir (os.path.dirname (sys.argv [0]))

folder = "photos"

class PhotosResource:
    def on_get (self, request, response):
        response.append_header ('Access-Control-Allow-Origin', '*')
        if not os.path.exists (folder):
            os.mkdir (folder)
        unsorted_list = [os.path.join (folder, each) for each in os.listdir (folder)]
        sorted_list = sorted (unsorted_list, key = os.path.getmtime)
        sorted_list.reverse ()
        response.body = json.dumps (sorted_list)

subprocess.call (["xset", "s", "noblank"])
subprocess.call (["xset", "s", "off"])
subprocess.call (["xset", "-dpms"])
subprocess.Popen (["chromium", "elm-python-pi-tv.html"])
time.sleep (10)
subprocess.call (["xdotool", "search", "--class", "chromium", "key", "F11"])

api = falcon.API ()
api.add_route (os.path.sep + folder, PhotosResource ())
server = simple_server.make_server ("127.0.0.1", 3000, api)
server.serve_forever ()
