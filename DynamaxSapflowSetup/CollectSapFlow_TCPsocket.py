#!/bin/env/python3

# This script establishes the data transfer between a Campbell logger Serial interface and a Computer through a Elfin EE10 RS232-Ethernet convertor running a TCP socket server.
# Data will be continously logged, and the full table will be backedup at midnight and before each (re)start. If connection is lost, the script will continously try to reconnect.
# Attention: If a reconnect occurs, the full table backup will be appended to the day's backupfile. Be sure to remove dupilcates when analysing the files.

# Code by David Basler 2021
import sys
import socket
from datetime import date

HOST = '192.168.0.58'  # The server's hostname or IP address
PORT = 8899            # The port used by the server
LOGGER="TEST"

# Get all commandline arguments
if len(sys.argv)<4:
	sys.exit("usage python %s [HOST] [PORT] [LOGGER]",)
	HOST=sys.argv[1]
	PORT=sys.argv[2]
	LOGGER=sys.argv[3]

interval=15 #in seconds
bufferlen=1024

outfilename= "%s_data.txt" % LOGGER
timeout=interval+5
maxlost=3
lost=0
dt = date.today()

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

def getAll(outfilename):
	s.settimeout(3.0)
	#Request All data
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
			#print (data)
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
			backupfilename= "%s_backup_%s.txt" % (LOGGER,dt.strftime("%Y-%m-%d"))
			getAll(backupfilename)
			return


reconnect()

while 1:
	s.settimeout(timeout)
	data=''
	try:
		data = s.recv(bufferlen)
	except socket.timeout:
		lost=lost+1
		print("no data")
	except ConnectionResetError:
		reconnect()
	except ConnectionAbortedError:
		reconnect()
	if lost>maxlost:
		reconnect()
		lost=0
	if len(data) > 0:
		print (data)
		f=open(outfilename,'a',newline='')
		data=data.decode().replace("\r","")
		f.write(data)
		f.close()
	if not dt==date.today():
		backupfilename= "%s_backup_%s.txt" % (LOGGER,dt.strftime("%Y-%m-%d"))
		getAll(backupfilename)
		dt = date.today()

