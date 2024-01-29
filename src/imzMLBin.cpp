/*************************************************************************
 *     rMSI - R package for MSI data processing
 *     Copyright (C) 2019 Pere Rafols Soler
 * 
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 * 
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 * 
 *     You should have received a copy of the GNU General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 **************************************************************************/

#include "imzMLBin.h"
#include <stdexcept>
#include <future>
#include <chrono>
#include <Rcpp.h>

//#define __DEBUG__
#define INTERPOLATION_TIMEOUT 10 //Timeout for interpolation theads ins ms

ImzMLBin::ImzMLBin(const char* ibd_fname,  unsigned int num_of_pixels,Rcpp::String Str_mzType, Rcpp::String Str_intType, bool continuous, Mode mode):
  ibdFname(ibd_fname), Npixels(num_of_pixels), bContinuous(continuous), fileMode(mode)
{
  
  mzDataType = string2imzMLDatatype(Str_mzType);
  intDataType = string2imzMLDatatype(Str_intType);
  
#ifdef __DEBUG__
  Rcpp::Rcout << "ImzMLBin::ImzMLBin() entring... with " << Npixels << " num. of pixels...";
#endif
  
  switch(mzDataType)
  {
    case int32: 
    case float32:  
      mzDataPointBytes = 4;
      break;
      
    case int64:
    case float64:
      mzDataPointBytes = 8;
      break;

    default:
      throw std::runtime_error("ERROR: ImzMLBin class constructor: invalid mzType ");
      break;
  }
  
  switch(intDataType)
  {
    case int32: 
    case float32:  
      intDataPointBytes = 4;
      break;
      
    case int64:
    case float64:
      intDataPointBytes = 8;
      break;
      
    default:
      throw std::runtime_error("ERROR: ImzMLBin class constructor: invalid intType.\n");
      break;
  }
  

#ifdef __DEBUG__
Rcpp::Rcout << "ImzMLBin() constructor end successfuly\n";
#endif
}

ImzMLBin::~ImzMLBin()
{
  close();
  
#ifdef __DEBUG__
  Rcpp::Rcout << "ImzMLBin() destructor end successfuly\n";
#endif
}

const char* ImzMLBin::getIbdFilePath()
{
  return ibdFname.get_cstring();
}

bool ImzMLBin::get_continuous()
{
  return bContinuous;
}

unsigned int ImzMLBin::get_mzEncodingBytes()
{
  return mzDataPointBytes;
}

unsigned int ImzMLBin::get_intEncodingBytes()
{
  return intDataPointBytes;
}

unsigned int ImzMLBin::get_number_of_pixels()
{
  return Npixels;  
}

void ImzMLBin::close()
{
#ifdef __DEBUG__
  Rcpp::Rcout << "ImzMLBin() destructor start...";
#endif
  if(ibdFile.is_open())
  {
    ibdFile.close();
#ifdef __DEBUG__
    Rcpp::Rcout << "ibdFile closed...\n";
#endif
  }
}

ImzMLBin::imzMLDataType ImzMLBin::string2imzMLDatatype(Rcpp::String data_type)
{
  imzMLDataType dataType;
  if( data_type == "float" )
  {
    dataType = imzMLDataType::float32;
  }
  else if( data_type == "double" )
  {
    dataType = imzMLDataType::float64;
  }
  else if( data_type == "int" )
  {
    dataType = imzMLDataType::int32;
  }
  else if( data_type == "long" )
  {
    dataType = imzMLDataType::int64;
  }
  else
  {
    throw std::runtime_error("ERROR: string2imzMLDatatype() invalid imzML datatype.\n");
  }
  
  return dataType;
}

template<typename T> 
void ImzMLBin::convertBytes2Double(char* inBytes, double* outPtr, unsigned int N)
{
  //First, copy the data to an intermediate vector with the desired type
  T* auxBuffer = new T[N];
  memcpy(auxBuffer, inBytes, sizeof(T)*N);
  
  //Finally, move the data to the double pointer
  for(int i = 0; i < N; i++)
  {
    outPtr[i] = (double)auxBuffer[i];
  }
  
  delete[] auxBuffer;
}

template<typename T> 
void ImzMLBin::convertDouble2Bytes(double* inPtr, char* outBytes, unsigned int N)
{
  //First, copy the data to an intermediate vector with the desired type
  T* auxBuffer = new T[N];
  for(int i = 0; i < N; i++)
  {
    auxBuffer[i] = (T)inPtr[i]; //Conversion from double to T type
  }
  
  //Finally, move the data to the output bytes buffer
  memcpy(outBytes, auxBuffer, sizeof(T)*N);
  
  delete[] auxBuffer;
}

ImzMLBinRead::ImzMLBinRead(const char* ibd_fname, unsigned int num_of_pixels, Rcpp::String Str_mzType, Rcpp::String Str_intType, bool continuous, bool openIbd, bool peakListrMSIformat):
  ImzMLBin(ibd_fname, num_of_pixels, Str_mzType, Str_intType, continuous, Mode::Read)
{
  pixels_read_offsets.resize(num_of_pixels);
  
  if(openIbd)
  {
    open();
  }
  
#ifdef __DEBUG__
  Rcpp::Rcout << "ImzMLBinRead() constructor end successfuly\n";
#endif
}

ImzMLBinRead::~ImzMLBinRead()
{
  //Empty desctructor
}

void ImzMLBinRead::open()
{
#ifdef __DEBUG__
  Rcpp::Rcout << "ImzMLBinRead() open start...\nibdfile is:"<<  ibdFname.get_cstring() << "\n";
#endif
  ibdFile.open(ibdFname.get_cstring(), std::fstream::in | std::ios::binary);
  if(!ibdFile.is_open())
  {
    throw std::runtime_error("ERROR: ImzMLBinRead could not open the imzML ibd file.\n"); 
  }
}

void ImzMLBinRead::readDataCommon(std::streampos offset, unsigned int N, double* ptr, unsigned int dataPointBytes, imzMLDataType dataType)
{
  unsigned int byteCount = N*dataPointBytes;
  char* buffer = new char [byteCount];
  
  if(offset >= 0)
  {
    ibdFile.seekg(offset);
    if(ibdFile.eof())
    {
      throw std::runtime_error("ERROR: ImzMLBinRead reached EOF seeking the imzML ibd file.\n"); 
    }
    if(ibdFile.fail() || ibdFile.bad())
    {
      throw std::runtime_error("FATAL ERROR: ImzMLBinRead got fail or bad bit condition seeking the imzML ibd file.\n"); 
    }
  }
  
  ibdFile.read (buffer, byteCount);
  if(ibdFile.eof())
  {
    throw std::runtime_error("ERROR: ImzMLBinRead reached EOF reading the imzML ibd file.\n"); 
  }
  if(ibdFile.fail() || ibdFile.bad())
  {
    throw std::runtime_error("FATAL ERROR: ImzMLBinRead got fail or bad bit condition reading the imzML ibd file.\n"); 
  }
  
  switch(dataType)
  {
  case int32:
    convertBytes2Double<int32_t>(buffer, ptr, N);
    break;
    
  case float32:  
    convertBytes2Double<float>(buffer, ptr, N);
    break;
    
  case int64:
    convertBytes2Double<int64_t>(buffer, ptr, N);
    break;
    
  case float64:
    //convertBytes2Double<double>(buffer, ptr, N); If double there is no need of intermediate conversion
    memcpy(ptr, buffer, sizeof(double)*N);
    break;
  }
  
  delete[] buffer;
}


void ImzMLBinRead::readIntData(std::streampos offset, unsigned int N, double* ptr )
{
  readDataCommon(offset, N, ptr, intDataPointBytes, intDataType);
}




///R METHODS////////////////////////////////////////////////////////////////////////

//' Generic method for the imzMLreader
//' testingimzMLBinRead
//' @param ibdFname: full path to the ibd file.
//' @param NPixels: Total number of pixels in the image.
//' @param N: number of elemetns (or data point to read).
//' @param offset: offset in bytes at which the reading operation is started.
//' @param read_mz: if true m/z data is readed, otherwise intensities are readed.
//' @param continuous: true if imzML data is in continuous mode
// [[Rcpp::export]]
Rcpp::NumericVector imzMLBinReadGeneric(const char* ibdFname, unsigned int NPixels, unsigned int N, uint64_t offset, Rcpp::String dataTypeString, bool read_mz, bool continuous)
{
  Rcpp::NumericVector x(N);
  try
  {
    ImzMLBinRead myReader(ibdFname, NPixels, dataTypeString, dataTypeString, continuous);
    myReader.readIntData(offset, N, x.begin());
    myReader.close();
  }
  catch(std::runtime_error &e)
  {
    Rcpp::Rcout << "Catch Error: "<< e.what() << "\n";
  }

  return x;
}
