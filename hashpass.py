#!/usr/bin/python -O
import base64, getpass, hashlib

domain = raw_input('Domain: ').strip().lower()
key = getpass.getpass('Key: ')

bits = domain + '/' + key
for i in range(2 ** 16):
  bits = hashlib.sha256(bits).digest()
password = base64.b64encode(bits)[:16]

print('Password: ' + password)
