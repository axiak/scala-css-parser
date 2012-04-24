#!/bin/bash
curl "$1" | java -cp /opt/scala/lib/scala-library.jar:. Main 
