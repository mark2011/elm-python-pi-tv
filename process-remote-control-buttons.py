#!/usr/bin/python
import json, os, subprocess, string, sys, time
import falcon
from wsgiref import simple_server

os.chdir (os.path.dirname (sys.argv [0]))

#class PhotosResource:
#    def on_get (self, request, response):
#        response.append_header ('Access-Control-Allow-Origin', '*')
#        if not os.path.exists (folder):
#            os.mkdir (folder)
#        unsorted_list = [os.path.join (folder, each) for each in os.listdir (folder)]
#        sorted_list = sorted (unsorted_list, key = os.path.getmtime)
#        sorted_list.reverse ()
#        response.body = json.dumps (sorted_list)

cec = subprocess.Popen (["cec-client"], stdout=subprocess.PIPE)
xdotool = subprocess.Popen (["xdotool", "-"], stdin=subprocess.PIPE)
#subprocess.call (["xdotool", "search", "--class", "chromium", "key", "F11"])

def send_key (line, key):
    print "key", key, "line", line
    xdotool.stdin.write ("search --class chromium key " + key + "\n")
while True:
    cec_line = cec.stdout.readline ().rstrip ()
    if cec_line.find ("key pressed: left") != -1:
       send_key (cec_line, 'h')
    elif cec_line.find ("key pressed: right") != -1:
       send_key (cec_line, 'l')
    elif cec_line.find ("key pressed: rewind") != -1:
       send_key (cec_line, '0')
    elif cec_line.find ("key pressed") != -1:
       print cec_line

#api = falcon.API ()
#api.add_route (os.path.sep + folder, PhotosResource ())
#server = simple_server.make_server ("127.0.0.1", 3000, api)
#server.serve_forever ()
