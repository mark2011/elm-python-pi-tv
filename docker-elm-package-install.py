#!/usr/bin/env python2.7

# This script is a hack until the elm-package install bug is fixed in docker containers
import urllib2
import argparse
import tempfile
import os
import zipfile
import shutil
import json

def unzip(source_filename, dest_dir):
    print "Unzipping to %s" % (dest_dir)
    fh = open(source_filename, 'rb')
    z = zipfile.ZipFile(fh)
    for name in z.namelist():
        z.extract(name, dest_dir)
    fh.close()

def downloadFile(url, fileName):
    u = urllib2.urlopen(url)
    f = open(fileName, 'wb')
    meta = u.info()
    file_size = int(meta.getheaders("Content-Length")[0])
    print "Downloading from %s to %s (Total Bytes: %i)" % (url, fileName, file_size)

    file_size_dl = 0
    block_sz = 8192
    while True:
        buffer = u.read(block_sz)
        if not buffer:
            break

        file_size_dl += len(buffer)
        f.write(buffer)
        status = r"%10d  [%3.2f%%]" % (file_size_dl, file_size_dl * 100. / file_size)
        status = status + chr(8)*(len(status)+1)
        print status,
    print ""
    f.close()

def downloadFileRetrying(url, fileName):
    for attempt in range(10):
        try:
            downloadFile(url, fileName)
        except Exception as e:
            print "Download number %i failed: %s" % (attempt, e)
            if attempt == 9:
                raise Exception('Download of ' + url + ' finally failed.')
            continue
        break

def findPackageJson(d):
    directories = []
    for f in os.listdir(d):
        fullPath = os.path.join(d, f)
        if os.path.isdir(fullPath):
            directories.append(fullPath)
        if f == "elm-package.json":
            return d
    for d in directories:
        ret = findPackageJson(d)
        if ret != None:
            return ret
    return None

def writeExactDeps(exactDepsFile, package, vers):
    print "Adding to %s file: %s %s" % (exactDepsFile, package, vers)
    if os.path.exists(exactDepsFile):
        with open(exactDepsFile, "r+") as f:
            x = json.load(f)
        with open(exactDepsFile, "w+") as f:
            x[package] = vers;
            json.dump(x, f)
    else:
        with open(exactDepsFile, "a+") as f:
            json.dump({ package: vers }, f)

def main():
    parser = argparse.ArgumentParser(description='(Hacky) alternative to elm-package install')
    parser.add_argument('package', metavar='PKG', type=str, help='Elm package name')
    parser.add_argument('version', metavar='VERS', type=str, help='Elm package version')
    args = parser.parse_args()

    elmDir = "elm-stuff"
    if not os.path.exists(elmDir):
        os.makedirs(elmDir)

    packageDir = os.path.join(elmDir, "packages", args.package, args.version)
    if os.path.exists(packageDir):
        shutil.rmtree(packageDir)

    exactDepsFile = os.path.join(elmDir, "exact-dependencies.json") 

    fd, filename = tempfile.mkstemp()
    dirpath = tempfile.mkdtemp()
    try:
        os.close(fd)
        downloadFileRetrying("http://github.com/" + args.package + "/zipball/" + args.version + "/", filename)
        unzip(filename, dirpath)
        zipSourceDir = findPackageJson(dirpath)
        print "Copying to %s" % (packageDir)
        shutil.copytree(zipSourceDir, packageDir)
        writeExactDeps(exactDepsFile, args.package, args.version)
    finally:
        os.remove(filename)
        shutil.rmtree(dirpath)
main()
