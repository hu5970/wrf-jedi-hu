/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include "RunWRFJEDI.h"
#include "oops/runs/HofX.h"
#include "ufo/instantiateObsOperatorFactory.h"
#include "WRFJEDITraits.h"

int main(int argc,  char ** argv) {
  wrfjedi::RunWRFJEDI run(argc, argv);
  ufo::instantiateObsOperatorFactory<wrfjedi::WRFJEDITraits>();
  oops::HofX<wrfjedi::WRFJEDITraits> hofx;

  run.execute(hofx);

  return 0;
};

