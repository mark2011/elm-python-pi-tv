#!/usr/bin/python
import os, subprocess, sys

os.chdir (os.path.dirname (sys.argv [0]))

cec = subprocess.Popen (["cec-client"], stdout=subprocess.PIPE)
xdotool = subprocess.Popen (["xdotool", "-"], stdin=subprocess.PIPE)

def send_key (line, key):
    print "key", key, "line", line
    xdotool.stdin.write ("search --class chromium key " + key + "\n")
while True:
    cec_line = cec.stdout.readline ().rstrip ()
    if cec_line.find ("key pressed: select") != -1:
       send_key (cec_line, 'y')
    elif cec_line.find ("key pressed: exit") != -1:
       send_key (cec_line, 'e')
    elif cec_line.find ("key pressed: left") != -1:
       send_key (cec_line, 'h')
    elif cec_line.find ("key pressed: right") != -1:
       send_key (cec_line, 'l')
    elif cec_line.find ("key pressed: up") != -1:
       send_key (cec_line, 'j')
    elif cec_line.find ("key pressed: down") != -1:
       send_key (cec_line, 'k')
    elif cec_line.find ("key pressed: rewind") != -1:
       send_key (cec_line, '0')
    elif cec_line.find ("key pressed") != -1:
       print cec_line
