#!/bin/bash
find package -name "*.roc" | entr -cr roc check package/Parser.roc
