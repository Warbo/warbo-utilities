#!/usr/bin/env python
import leveldb
import sys
if len(sys.argv) < 2: raise Exception('Please give leveldb dir as argument')
db = sys.argv[1]
sys.stderr.write('Attempting to repair leveldb at {0}\n'.format(db))
leveldb.RepairDB(db)
sys.stderr.write('Finished processing {0}\n'.format(db))
