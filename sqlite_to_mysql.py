#!/usr/bin/python -O

# stdin: sqlite SQL
# stdout: mysql SQL

import re
import sys

def replace(s, old, new):
  def replace_func(m):
    if m.group().startswith("'"):
      return m.group()
    return m.group().replace(old, new)
  return re.sub("'[^']*'|[^'\"]+", replace_func, s)

for line in sys.stdin.readlines():
  if line.upper().startswith("PRAGMA"):
    continue
  if line.upper().startswith("BEGIN TRANSACTION"):
    continue
  if line.upper().startswith("COMMIT"):
    continue
  if line.upper().startswith("DELETE FROM SQLITE_SEQUENCE"):
    continue
  if line.upper().startswith("INSERT INTO \"SQLITE_SEQUENCE\""):
    continue
  line = replace(line, "AUTOINCREMENT", "AUTO_INCREMENT")
  line = line.replace("'t'", "'1'")
  line = line.replace("'f'", "'0'")
  line = line.replace("\"", "`")
  sys.stdout.write(line)