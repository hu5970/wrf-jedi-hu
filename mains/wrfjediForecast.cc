/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include "WRFJEDITraits.h"
#include "oops/runs/Forecast.h"
#include "RunWRFJEDI.h"

int main(int argc,  char ** argv) {
  wrfjedi::RunWRFJEDI run(argc, argv);
  oops::Forecast<wrfjedi::WRFJEDITraits> fc;
  run.execute(fc);
  return 0;
}
