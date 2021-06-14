#!/bin/env/python3

# This script establishes the data transfer between a Campbell logger Serial interface and a Computer through a Elfin EE10 RS232-Ethernet convertor running a TCP socket server.
# Data will be continously logged, and the full table will be backedup at midnight and before each (re)start. If connection is lost, the script will continously try to reconnect.
# Attention: If a reconnect occurs, the full table backup will be appended to the day's backupfile. Be sure to remove dupilcates when analysing the files.

# Code by David Basler 2021
import sys
import socket
from datetime import datetime

HOST = '192.168.0.58'  # The server's hostname or IP address
PORT = 8899            # The port used by the server
LOGGER="TEST"

# Get all commandline arguments
if len(sys.argv)<4:
	sys.exit("usage python %s [HOST] [PORT] [LOGGER]",)

HOST=sys.argv[1]
PORT=int(sys.argv[2])
LOGGER=sys.argv[3]

bufferlen=1024

outfilename= "%s_data.txt" % LOGGER
timeout=20.0
maxlost=3
lost=0
dt = datetime.today()

print("%s@%s:%i" % (LOGGER,HOST,PORT))

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

def getAll(outfilename):
	s.settimeout(120.0)
	print("Request All data")
	s.sendall(b'#ALL')
	counter=0
	while 1:
		data=""
		try:
			data = s.recv(bufferlen)
		except socket.timeout:
			print ("Backed-up %s rows" % counter) 
			return
		if len(data) > 0:
			print (data)
			f=open(outfilename,'a',newline='')
			data=data.decode().replace("\r","")
			f.write(data)
			f.close()
			counter = counter + len(data.split('\n'))

def reconnect():
	global s
	while 1:
		s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		try:
			s.connect((HOST, PORT))
		except TimeoutError:
			print("Failed to open socket")
		except OSError:
			print("unreachable host")
		else:			
			print('Connected to TCPsocket %s:%s @%s' % (HOST,PORT,LOGGER))
			return

reconnect()

s.settimeout(timeout)
backupfilename= "%s_backup_%s.txt" % (LOGGER,dt.strftime("%Y-%m-%d_%H%M"))
getAll(backupfilename)
print ("all none")
s.close()
sys.exit()

