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
#include "mlinterp.hpp" //Used for linear interpolation
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
  
  Offsets.resize(Npixels);
    
#ifdef __DEBUG__
Rcpp::Rcout << "ImzMLBin() constructor end successfuly\n";
Rcpp::Rcout << "Offsets size: " << Offsets.size() << "\n";
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

unsigned int ImzMLBin::get_mzLength(unsigned int index)
{
  if(index >= Npixels)
  {
    throw std::runtime_error("ERROR: ImzMLBin class get_mzLength(): out of range.\n");
  }
  return Offsets[index].mzLength;
}

std::streampos ImzMLBin::get_mzOffset(unsigned int index)
{
  if(index >= Npixels)
  {
    throw std::runtime_error("ERROR: ImzMLBin class get_mzOffset(): out of range.\n");
  }
  return Offsets[index].mzOffset;
}

unsigned int ImzMLBin::get_intLength(unsigned int index)
{
  if(index >= Npixels)
  {
    throw std::runtime_error("ERROR: ImzMLBin class get_intLength(): out of range.\n");
  }
  return Offsets[index].intLength;
}

std::streampos ImzMLBin::get_intOffset(unsigned int index)
{
  if(index >= Npixels)
  {
    throw std::runtime_error("ERROR: ImzMLBin class get_intOffset(): out of range.\n");
  }
  return Offsets[index].intOffset;
}

Rcpp::DataFrame ImzMLBin::get_OffsetsLengths()
{
  Rcpp::NumericVector RmzLengths(Npixels);
  Rcpp::NumericVector RmzOffsets(Npixels);
  Rcpp::NumericVector RintLengths(Npixels);
  Rcpp::NumericVector RintOffsets(Npixels);
  
  for(unsigned int i = 0; i< Npixels; i++)
  {
    RmzLengths[i] = Offsets[i].mzLength;
    RmzOffsets[i] = Offsets[i].mzOffset;
    RintLengths[i] = Offsets[i].intLength;
    RintOffsets[i] = Offsets[i].intOffset;
  }
  
  return Rcpp::DataFrame::create( Rcpp::Named("mzLength") = RmzLengths,
                                  Rcpp::Named("mzOffset") = RmzOffsets,
                                  Rcpp::Named("intLength") = RintLengths,
                                  Rcpp::Named("intOffset") = RintOffsets
                                  );
}

void ImzMLBin::set_mzLength(Rcpp::NumericVector* mzLength_vector)
{
  if(fileMode == Mode::SequentialWriteFile)
  {
    throw std::runtime_error("ERROR: mzLength_vector cannot be set in sequential write mode.\n");
  }
  
  for(unsigned int i = 0; i < Npixels; i++)
  {
    Offsets[i].mzLength = (unsigned int)(*mzLength_vector)[i];
  }
}

void ImzMLBin::set_mzOffset(Rcpp::NumericVector* mzOffset_vector)
{
  if(fileMode == Mode::SequentialWriteFile)
  {
    throw std::runtime_error("ERROR: mzOffset_vector cannot be set in sequential write mode.\n");
  }
  
  for(unsigned int i = 0; i < Npixels; i++)
  {
    Offsets[i].mzOffset = (std::streampos)(*mzOffset_vector)[i];
  }
}

void ImzMLBin::set_intLength(Rcpp::NumericVector* intLength_vector)
{
  if(fileMode == Mode::SequentialWriteFile)
  {
    throw std::runtime_error("ERROR: intLength_vector cannot be set in sequential write mode.\n");
  }
  
  for(unsigned int i = 0; i < Npixels; i++)
  {
    Offsets[i].intLength = (unsigned int)(*intLength_vector)[i];
  }
}

void ImzMLBin::set_intOffset(Rcpp::NumericVector* intOffset_vector)
{
  if(fileMode == Mode::SequentialWriteFile)
  {
    throw std::runtime_error("ERROR: intOffset_vector cannot be set in sequential write mode.\n");
  }
  
  for(unsigned int i = 0; i < Npixels; i++)
  {
    Offsets[i].intOffset = (std::streampos)(*intOffset_vector)[i];
  }
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
  ImzMLBin(ibd_fname, num_of_pixels, Str_mzType, Str_intType, continuous, Mode::Read), bForceResampling(false), bOriginalMassAxisOnMem(false), bPeakListInrMSIFormat(peakListrMSIformat)
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

void ImzMLBinRead::readUUID(char* uuid)
{
  ibdFile.seekg(0);
  if(ibdFile.eof())
  {
    throw std::runtime_error("ERROR: ImzMLBinRead reached EOF seeking the imzML ibd file.\n"); 
  }
  if(ibdFile.fail() || ibdFile.bad())
  {
    throw std::runtime_error("FATAL ERROR: ImzMLBinRead got fail or bad bit condition seeking the imzML ibd file.\n"); 
  }
  
  ibdFile.read (uuid, 16);
  if(ibdFile.eof())
  {
    throw std::runtime_error("ERROR: ImzMLBin reached EOF reading the imzML ibd file.\n"); 
  }
  if(ibdFile.fail() || ibdFile.bad())
  {
    throw std::runtime_error("FATAL ERROR: ImzMLBin got fail or bad bit condition reading the imzML ibd file.\n"); 
  }
}

void ImzMLBinRead::readMzData(std::streampos offset, unsigned int N, double* ptr )
{
  if(get_continuous())
  {
    if( N  != get_mzLength(0))
    {
      throw std::runtime_error("ERROR: ImzMLBin tried to read the original mass axis with an invalid length.\n ");
    }
    
    //In continuous mode avoid re-reading the same offset
    if(!bOriginalMassAxisOnMem)
    {
      //First reading of the mass axis
      originalMassAxis.resize(N);
      readDataCommon(offset, N, originalMassAxis.data(), mzDataPointBytes, mzDataType);  
      bOriginalMassAxisOnMem = true;
      
      //Re-check if equals to the common mass axis
      checkCompareOriginalMassAxisAndCommonMassAxis();
    }
  
    //Mass axis already in mem so just copy from it
    memcpy( ptr, originalMassAxis.data(), N * sizeof(double) );
  }
  else
  {
    //In processd mode directley read to data pointer
    readDataCommon(offset, N, ptr, mzDataPointBytes, mzDataType);  
  }
}

void ImzMLBinRead::readIntData(std::streampos offset, unsigned int N, double* ptr )
{
  readDataCommon(offset, N, ptr, intDataPointBytes, intDataType);
}

// Set a common mass axis diferent than the original image mass axis. Thus, each readed spectrum will be interpolated to the common mass axis.
//commonMassLength: number of points in the common mass axis.
//commonMass: pointer to the common mass axis
void ImzMLBinRead::setCommonMassAxis(unsigned int commonMassLength, double *commonMass)
{
  commonMassAxis.resize(commonMassLength);
  memcpy( commonMassAxis.data(), commonMass, commonMassLength * sizeof(double) );
  
  //Check if the common mass axis is diferent than the original mass axis
  checkCompareOriginalMassAxisAndCommonMassAxis();
}

//Process both mass axis and compare them. If diferent, the bForceResampling flag will be set to true.
void ImzMLBinRead::checkCompareOriginalMassAxisAndCommonMassAxis()
{
  bForceResampling = false;  
  if( commonMassAxis.size() != originalMassAxis.size() )
  {
    bForceResampling = true;  
  }
  else
  {
    for(unsigned int i = 0; i < commonMassAxis.size(); i ++)
    {
      if(commonMassAxis[i] != originalMassAxis[i])
      {
        bForceResampling = true;
        break;
      }
    }
  } 
}

//Read a single spectrum from the imzML data
//If data is in processed mode the spectrum will be interpolated to the common mass axis
//pixelID: the pixel ID of the spectrum to read.
//ionIndex: the ion index at which to start reading the spectrum (0 means reading from the begining).
//ionCount: the number of mass channels to read (massLength means reading the whole spectrum).
//out: a pointer where data will be stored.
//bRunLinearInterpolationOnLoad: set this boolean to true to run linear interpolation on load automatically
//offset_proc_data: offset to start reading spectra in processed mode to skip all the inital mass axis. It will be modified with the last readed mass offset.
imzMLSpectrum ImzMLBinRead::ReadSpectrum(int pixelID, unsigned int ionIndex, unsigned int ionCount, double *out, 
                                         bool bRunLinearInterpolationOnLoad, unsigned int offset_proc_data)
{
  if(commonMassAxis.size() == 0)
  {
    throw std::runtime_error("Error: common mass axis not available. Set it using  ImzMLBinRead::setCommonMassAxis() method\n"); 
  }
    
  if( (ionIndex+ionCount) > commonMassAxis.size() )
  {
    throw std::runtime_error("Error: mass channels out of range\n"); 
  }
  
  imzMLSpectrum imzMLSpc;
  imzMLSpc.pixelID = pixelID;
  
  if(get_continuous() && !bOriginalMassAxisOnMem)
  {
    //Continuous mode but the original Mass axis is still not in memory
    std::vector<double> dummy_mass(get_mzLength(0));
    readMzData(get_mzOffset(0), get_mzLength(0), dummy_mass.data());
    dummy_mass.clear();
  }
  //Now it is ensured the orignial mass axis is on mem and the bForeceResampling is set properly
  
  if(get_continuous() && !bForceResampling)
  {
    //Continuous mode, just load the spectrum intensity vector
    readIntData(get_intOffset(pixelID) + (std::streampos)(ionIndex*get_intEncodingBytes()), ionCount, out);  
  }
  else
  {
    //Interpolation is needed because one of the followings:
    //  - Continuous mode with a different mass axis
    //  - Processed mode
    
    //Intermediate buffers to load data before interpolation
    if( get_mzLength(pixelID) != get_intLength(pixelID))
    {
      throw std::runtime_error("Error: different mass and intensity length in the imzML data\n"); 
    }
    
    //Read processed mode mass and intensity
    try
    {
      //Complex read by searching the apropiate mass
      double current_mass;
      unsigned int start_ioffset = offset_proc_data; //Offset at which start reading the spectrum
      unsigned int found_ioffset = start_ioffset;
      unsigned int last_ioffset = start_ioffset;
      
      if(!get_continuous())
      {
        ibdFile.seekg(get_mzOffset(pixelID) +  (std::streampos)(start_ioffset*get_mzEncodingBytes()));
        if(ibdFile.eof())
        {
          throw std::runtime_error("ERROR: mass look-up reached EOF seeking the imzML ibd file.\n"); 
        }
        if(ibdFile.fail() || ibdFile.bad())
        {
          throw std::runtime_error("FATAL ERROR: mass look-up got fail or bad bit condition seeking the imzML ibd file.\n"); 
        }
      }
      
      //Search the first mass channel
      unsigned int ioffset = start_ioffset;
      while( ioffset < get_mzLength(pixelID) )
      {
        if(get_continuous())
        {
          current_mass = originalMassAxis[ioffset];
        }
        else
        {
          readMzData(-1, 1, &current_mass); //Read a single mass channel
        }
        if( current_mass >= commonMassAxis[ionIndex])
        {
          break;
        }
        found_ioffset = ioffset;
        ioffset++;
      }
      
      //Read the mass channel till get the last
      if(!get_continuous())
      {
        ibdFile.seekg(get_mzOffset(pixelID) +  (std::streampos)(found_ioffset*get_mzEncodingBytes()));
        if(ibdFile.eof())
        {
          throw std::runtime_error("ERROR: mass read reached EOF seeking the imzML ibd file.\n"); 
        }
        if(ibdFile.fail() || ibdFile.bad())
        {
          throw std::runtime_error("FATAL ERROR: mass read got fail or bad bit condition seeking the imzML ibd file.\n"); 
        }
      }
      ioffset = found_ioffset;
      while( ioffset < get_mzLength(pixelID) )
      {
        if(get_continuous())
        {
          current_mass = originalMassAxis[ioffset];
        }
        else
        {
          readMzData(-1, 1, &current_mass); //Read a single mass channel
        }
        imzMLSpc.imzMLmass.push_back(current_mass);
        if( current_mass > commonMassAxis[ionIndex + ionCount -1] )
        {
          break;
        }
        last_ioffset = ioffset;
        ioffset++;
      }
      
      imzMLSpc.last_offset = last_ioffset;
      imzMLSpc.imzMLintensity.resize(imzMLSpc.imzMLmass.size());
      
      //Read the corresponding intensity
      readIntData(get_intOffset(pixelID) + (std::streampos)(found_ioffset*get_intEncodingBytes()), imzMLSpc.imzMLintensity.size(), imzMLSpc.imzMLintensity.data());
    }
    catch(std::runtime_error &e)
    {
      throw std::runtime_error(e.what());
    }
    
    //Linear interpolation
    if(bRunLinearInterpolationOnLoad)
    {
      InterpolateSpectrum(&imzMLSpc, ionIndex, ionCount, out);
    }
  }
  
  return imzMLSpc;
}

void ImzMLBinRead::ReadSpectra(unsigned int numOfPixels, unsigned int *pixelIDs, unsigned int ionIndex, unsigned int ionCount, double *out, unsigned int number_of_threads, bool bUpdate_pixel_read_offsets)
{
  ReadSpectraTemplateType<double>(numOfPixels, pixelIDs, ionIndex, ionCount, out, number_of_threads, bUpdate_pixel_read_offsets);
}

void ImzMLBinRead::ReadSpectra(unsigned int numOfPixels, unsigned int *pixelIDs, double *scaling_factors, unsigned int ionIndex, unsigned int ionCount, imgstreamencoding_type *out, unsigned int number_of_threads, bool bUpdate_pixel_read_offsets)
{
  ReadSpectraTemplateType<imgstreamencoding_type>(numOfPixels, pixelIDs, ionIndex, ionCount, out, number_of_threads, bUpdate_pixel_read_offsets, scaling_factors);
}

template<typename T>
void ImzMLBinRead::ReadSpectraTemplateType(unsigned int numOfPixels, unsigned int *pixelIDs, unsigned int ionIndex, unsigned int ionCount, T *out, unsigned int number_of_threads, bool bUpdate_pixel_read_offsets, double *scaling_factors)
{
  if(typeid(T) != typeid(imgstreamencoding_type) && typeid(T) != typeid(double))
  {
    throw std::runtime_error("Error in ReadSpectraTemplateType: invalid T type.");
  }
  
  std::vector<imzMLSpectrum> thread_spectrum(number_of_threads);
  std::vector<std::future<void>> futures(number_of_threads);
  unsigned int current_pixel = 0;
  std::chrono::milliseconds timeout(INTERPOLATION_TIMEOUT);
  std::vector<double*> thread_readingBuffers(number_of_threads);
  std::vector<imgstreamencoding_type*> thread_outputBuffers(number_of_threads);
  
  while(true)
  {
    //Start threads
    for( int ithread = 0; ithread < number_of_threads; ithread++)
    {
      if(!futures[ithread].valid() && current_pixel < numOfPixels)
      {
        if(typeid(T) == typeid(imgstreamencoding_type))
        {
          //Using imgstreamencoding_type as output buffer (this happens during pngstream encoding)
          thread_readingBuffers[ithread] = new double[ionCount];
          thread_outputBuffers[ithread] = (imgstreamencoding_type *)(out + (current_pixel*ionCount));
        }
        else
        {
          //Using double as output buffer (this happens during spectral data loading)
          thread_readingBuffers[ithread] = (double *)(out + (current_pixel*ionCount));
        }
        
        thread_spectrum[ithread] = ReadSpectrum( pixelIDs[current_pixel], ionIndex, ionCount, thread_readingBuffers[ithread], false, pixels_read_offsets[ pixelIDs[current_pixel]]);
        
        
        if(bUpdate_pixel_read_offsets)
        {
          pixels_read_offsets[ pixelIDs[current_pixel]] =  thread_spectrum[ithread].last_offset;
        }

        //Only run threads for data in processed mode or resampling
        futures[ithread] = std::async(std::launch::async, &ImzMLBinRead::InterpolateSpectrum, this, &thread_spectrum[ithread], ionIndex, ionCount, thread_readingBuffers[ithread]);
        current_pixel++;
      }
    }
    
    //Wait for threads to finish
    for( int ithread = 0; ithread < number_of_threads; ithread++)
    {
      if(futures[ithread].valid() && (futures[ithread].wait_for(timeout) == std::future_status::ready) )
      {
        futures[ithread].get();
        
        if(typeid(T) == typeid(imgstreamencoding_type))
        {
          //Copy to imgstreamencoding_type buffer output using the scaling factors
          for(unsigned int i = 0; i < ionCount; i++)
          {
            if(scaling_factors[i + ionIndex] > 0 )
            {
              thread_outputBuffers[ithread][i] = ENCODING_BIT_MASK & (imgstreamencoding_type)(ENCODER_RANGE*((thread_readingBuffers[ithread][i]/scaling_factors[i + ionIndex]))); //apply scalling and adjust dynamic range 
            }
            else
            {
              thread_outputBuffers[ithread][i] = 0;
            }
          }
          delete[] thread_readingBuffers[ithread];
        }
      }
    }
    
    //Calculate the end condition
    if(current_pixel >= numOfPixels)
    {
      bool bExit = true;
      for( int ithread = 0; ithread < number_of_threads; ithread++)
      {
        bExit &= !futures[ithread].valid();
      }
      if(bExit)
      {
        return;
      }
    }
  }
}

//imzMLSpc: pointer to a spectrum already read from the imzML file.
//ionIndex: the ion index at which to start reading the spectrum (0 means reading from the begining).
//ionCount: the number of mass channels to read (massLength means reading the whole spectrum).
//out: a pointer where data will be stored.
void ImzMLBinRead::InterpolateSpectrum(imzMLSpectrum *imzMLSpc, unsigned int ionIndex, unsigned int ionCount, double *out)
{
  if(!get_continuous() || bForceResampling) 
  {
    //Only inpterpolate for data in processed mode or different mass axis in continuous mode
    
    //Linear interpolation
    const int massLength = imzMLSpc->imzMLmass.size();
    if(massLength > 0)
    {
      mlinterp::interp(
        &massLength, (int)ionCount, // Number of points (imzML original, interpolated )
        imzMLSpc->imzMLintensity.data(), out, // Y axis  (imzML original, interpolated )
        imzMLSpc->imzMLmass.data(), commonMassAxis.data() + ionIndex // X axis  (imzML original, interpolated )
      );
    }
    else
    {
      for(unsigned int i = 0; i < ionCount; i++)
      {
        out[i] = 0; //Just set to zero if no spectral data avaialble
      }
    }
  }
}

PeakPicking::Peaks *ImzMLBinRead::ReadPeakList(int pixelID)
{
  if(get_continuous())
  {
    throw std::runtime_error("Error: trying to read a peak list from imzML in continuous mode.\n");
  }
  
  const int massLength = get_mzLength(pixelID);
  if( massLength != get_intLength(pixelID))
  {
    throw std::runtime_error("Error: different mass and intensity length in the imzML data\n"); 
  }
  
  PeakPicking::Peaks *mPeaks = new PeakPicking::Peaks;
  mPeaks->mass.resize(massLength);
  mPeaks->intensity.resize(massLength);
  if(bPeakListInrMSIFormat)
  {
    mPeaks->area.resize(massLength);
    mPeaks->SNR.resize(massLength);
    mPeaks->binSize.resize(massLength);
  }
  
  std::streampos rMSIPeakDataOffset;
  //Read peak list data
  try
  {
    readMzData(get_mzOffset(pixelID),  mPeaks->mass.size(), mPeaks->mass.data());
    readIntData(get_intOffset(pixelID), mPeaks->intensity.size(), mPeaks->intensity.data());
    if(bPeakListInrMSIFormat)
    {
      rMSIPeakDataOffset = get_intOffset(pixelID) + (std::streampos)(massLength*intDataPointBytes);
      readIntData(rMSIPeakDataOffset, mPeaks->area.size(), mPeaks->area.data());
      rMSIPeakDataOffset += (std::streampos)(massLength*intDataPointBytes);
      readIntData(rMSIPeakDataOffset, mPeaks->SNR.size(), mPeaks->SNR.data());
      rMSIPeakDataOffset += (std::streampos)(massLength*intDataPointBytes);
      readIntData(rMSIPeakDataOffset, mPeaks->binSize.size(), mPeaks->binSize.data());
    }
  }
  catch(std::runtime_error &e)
  {
    throw std::runtime_error(e.what());
  }
  
  return mPeaks;
}

bool ImzMLBinRead::get_rMSIPeakListFormat()
{
  return bPeakListInrMSIFormat;
}

ImzMLBinWrite::ImzMLBinWrite(const char* ibd_fname,  unsigned int num_of_pixels, Rcpp::String Str_mzType, Rcpp::String Str_intType, bool continuous, bool sequentialMode, bool openIbd) :
  ImzMLBin(ibd_fname, num_of_pixels, Str_mzType, Str_intType, continuous, sequentialMode? Mode::SequentialWriteFile : Mode::ModifyFile ),
  sequentialWriteIndex_IntData(0), 
  sequentialWriteIndex_MzData(0)
{
  if(openIbd)
  {
    open();
  }
}

ImzMLBinWrite::~ImzMLBinWrite()
{
  //Empty desctructor
}

void ImzMLBinWrite::open(bool truncate)
{
  if(fileMode == Mode::ModifyFile)
  {
    //Open for modifing registers in the ibd file
    ibdFile.open(ibdFname.get_cstring(), std::fstream::in | std::fstream::out | std::ios::binary); 
  }
  else
  {
    if(truncate)
    {
      //Open for serial writing, any content of the ibd file will be removed
      ibdFile.open(ibdFname.get_cstring(), std::fstream::out | std::ios::binary | std::fstream::trunc);
    }
    else
    {
      //Open for serial writing, append data to the end
      ibdFile.open(ibdFname.get_cstring(), std::fstream::out | std::ios::binary | std::fstream::app );
    }
  }
  
  if(!ibdFile.is_open())
  {
    throw std::runtime_error("Error: ImzMLBinWrite could not open the imzML ibd file.\n");
  }
}

void ImzMLBinWrite::writeUUIDBytes(const char* uuid)
{
  if(ibdFile.tellp() != 0)
  {
    throw std::runtime_error("ERROR: the ibd writing pointer is not at zero so UUID cannot be writen");
  }
  
  ibdFile.write (uuid, 16);
  if(ibdFile.fail() || ibdFile.bad())
  {
    throw std::runtime_error("FATAL ERROR: ImzMLBinWrite got fail or bad bit condition writing the imzML ibd file.\n"); 
  }
}

void ImzMLBinWrite::writeUUID(std::string suuid)
{
  //Convert UUID in string format to byte array
  char uuid[16];
  for( int i = 0; i < 16; i++)
  {
    uuid[i] = strtol(suuid.substr(i*2, 2).c_str(), NULL, 16);
  }
  writeUUIDBytes(uuid);
}

void ImzMLBinWrite::overwriteUUID(std::string suuid)
{
  if(fileMode == Mode::SequentialWriteFile)
  {
    throw std::runtime_error("ERROR: ibd file was opened in an invalid mode for data modification");
  }
  
  ibdFile.seekp(0);
  if(ibdFile.fail() || ibdFile.bad())
  {
    throw std::runtime_error("FATAL ERROR: overwriteUUID got fail or bad bit condition seeking the imzML ibd file.\n"); 
  }
  
  writeUUID(suuid);
}

void ImzMLBinWrite::writeMzData(std::streampos offset, unsigned int N, double* ptr )
{
  if(fileMode == Mode::SequentialWriteFile)
  {
    throw std::runtime_error("ERROR: ibd file was opened in an invalid mode for data modification");
  }
  
  writeDataCommon(offset, N, ptr, mzDataPointBytes, mzDataType);
}

void ImzMLBinWrite::writeMzData(unsigned int N, double* ptr )
{
  if(fileMode == Mode::ModifyFile)
  {
    throw std::runtime_error("ERROR: ibd file was opened in an invalid mode for sequencial writing");
  }
  
  //Store the mz offset and length
  if(sequentialWriteIndex_MzData >= Npixels)
  {
    throw std::runtime_error("ERROR: trying to write more spectral data than the maximum number of pixels set in the constructor");
  }
  
  //In continuos mode the offset and length for the mass axis is the first, so just replicate it
  if(get_continuous())
  {
    if(sequentialWriteIndex_MzData == 0)
    {
      //First mass axis write in continuous mode, so write it to hdd
      //In processed mode, just write and update indices
      Offsets[sequentialWriteIndex_MzData].mzOffset = ibdFile.tellp();
      Offsets[sequentialWriteIndex_MzData].mzLength = N;
      
      //Write data
      writeDataCommon(N, ptr, mzDataPointBytes, mzDataType);
    }
    else
    {
      //Just update indices
      Offsets[sequentialWriteIndex_MzData].mzOffset =  Offsets[0].mzOffset;
      Offsets[sequentialWriteIndex_MzData].mzLength = Offsets[0].mzLength;
    }
  }
  else
  {
    //In processed mode, just write and update indices
    Offsets[sequentialWriteIndex_MzData].mzOffset = ibdFile.tellp();
    Offsets[sequentialWriteIndex_MzData].mzLength = N;
  
    //Write data
    writeDataCommon(N, ptr, mzDataPointBytes, mzDataType);
  }
  
  sequentialWriteIndex_MzData++;
}

void ImzMLBinWrite::writeIntData(std::streampos offset, unsigned int N, double* ptr )
{
  if(fileMode == Mode::SequentialWriteFile)
  {
    throw std::runtime_error("ERROR: ibd file was opened in an invalid mode for data modification");
  }
  
  writeDataCommon(offset, N, ptr, intDataPointBytes, intDataType);
}

void ImzMLBinWrite::writeIntData(unsigned int N, double* ptr )
{
  if(fileMode == Mode::ModifyFile)
  {
    throw std::runtime_error("ERROR: ibd file was opened in an invalid mode for sequencial writing");
  }
  
  //Store the mz offset and length
  if(sequentialWriteIndex_IntData >= Npixels)
  {
    throw std::runtime_error("ERROR: trying to write more spectral data than the maximum number of pixels set in the constructor");
  }
 
  Offsets[sequentialWriteIndex_IntData].intOffset = ibdFile.tellp();
  Offsets[sequentialWriteIndex_IntData].intLength = N;
  /* DEBUG Prints must be commented out
  Rcpp::Rcout<<"DBG TRAP in writeIntData(): sequentialWriteIndex_IntData = " <<  sequentialWriteIndex_IntData << "\n";
  Rcpp::Rcout<<"DBG TRAP in writeIntData(): Offsets[sequentialWriteIndex_IntData].intOffset  = " << Offsets[sequentialWriteIndex_IntData].intOffset  << " \n";
  Rcpp::Rcout<<"DBG TRAP in writeIntData(): ibdFile.tellp() = " << ibdFile.tellp() << " \n";
  Rcpp::Rcout<<"DBG TRAP in writeIntData(): Offsets[sequentialWriteIndex_IntData].intLength = " << Offsets[sequentialWriteIndex_IntData].intLength << " \n";
  Rcpp::Rcout<<"DBG TRAP in writeIntData(): N = " << N << " \n";
  */
  sequentialWriteIndex_IntData++;
  
  //Write data
  writeDataCommon(N, ptr, intDataPointBytes, intDataType);
}

void ImzMLBinWrite::writePeakList( unsigned int N, double* ptrMass, double* ptrIntensity, double* ptrArea, double* ptrSNR, double* ptrBinSize)
{
  if(get_continuous())
  {
    throw std::runtime_error("ERROR: peaklist are only supported for imzML in processed mode");
  }
  
  if(fileMode == Mode::ModifyFile)
  {
    throw std::runtime_error("ERROR: ibd file was opened in an invalid mode for sequencial writing");
  }
  
  //Check offsets and length
  if(sequentialWriteIndex_MzData >= Npixels || sequentialWriteIndex_IntData >= Npixels)
  {
    throw std::runtime_error("ERROR: trying to write more spectral data than the maximum number of pixels set in the constructor");
  }
  
  //Update indices for the peak masses
  Offsets[sequentialWriteIndex_MzData].mzOffset = ibdFile.tellp();
  Offsets[sequentialWriteIndex_MzData].mzLength = N;
  
  //Write peak masses
  writeDataCommon(N, ptrMass, mzDataPointBytes, mzDataType);
  sequentialWriteIndex_MzData++;
  
  //Update indices for the peak data
  Offsets[sequentialWriteIndex_IntData].intOffset = ibdFile.tellp();
  Offsets[sequentialWriteIndex_IntData].intLength = N;
  
  //Write peakData
  writeDataCommon(N, ptrIntensity, intDataPointBytes, intDataType);
  writeDataCommon(N, ptrArea, intDataPointBytes, intDataType);
  writeDataCommon(N, ptrSNR, intDataPointBytes, intDataType);
  writeDataCommon(N, ptrBinSize, intDataPointBytes, intDataType);
  sequentialWriteIndex_IntData++;
}

void ImzMLBinWrite::writeDataCommon(std::streampos offset, unsigned int N, double* ptr, unsigned int dataPointBytes, imzMLDataType dataType)
{
  ibdFile.seekp(offset);
  if(ibdFile.fail() || ibdFile.bad())
  {
    throw std::runtime_error("FATAL ERROR: ImzMLBinWrite got fail or bad bit condition seeking the imzML ibd file.\n"); 
  }
  
  writeDataCommon(N, ptr, dataPointBytes, dataType);
}

void ImzMLBinWrite::writeDataCommon(unsigned int N, double* ptr, unsigned int dataPointBytes, imzMLDataType dataType)
{
  if( N == 0)
  {
    //If data length is zero just dont try to save anything. This may be the case for an empty peak list. Offsets will be set but nothing stored
    return;
  }
  
  unsigned int byteCount = N*dataPointBytes;
  char* buffer = new char [byteCount];
  
  //copy the ptr contents to the wrting buffer in the apropiate format 
  switch(dataType)
  {
    case int32:
      convertDouble2Bytes<int32_t>(ptr, buffer, N);
      break;
      
    case float32:  
      convertDouble2Bytes<float>(ptr, buffer, N);
      break;
      
    case int64:
      convertDouble2Bytes<int64_t>(ptr, buffer, N);
      break;
      
    case float64:
      //convertDouble2Bytes<double>(ptr, buffer, N); If double there is no need of intermediate conversion
      memcpy(buffer, ptr, sizeof(double)*N);
      break;
  }
  
  ibdFile.write (buffer, byteCount);
  if(ibdFile.fail() || ibdFile.bad())
  {
    throw std::runtime_error("FATAL ERROR: ImzMLBinWrite got fail or bad bit condition writing the imzML ibd file.\n"); 
  }
  
  delete[] buffer;
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
    if(read_mz)
    {
      myReader.readMzData(offset, N, x.begin());  
    }
    else
    {
      myReader.readIntData(offset, N, x.begin());
    }
    myReader.close();
    
  }
  catch(std::runtime_error &e)
  {
    Rcpp::Rcout << "Catch Error: "<< e.what() << "\n";
  }

  return x;
}

//' Testing the imzMLwriter in sequential mode
//' This function creates a new ibd file with the provided data descibed in the following params
//' @param ibdFname: full path to the ibd file.
//' @param mz_dataTypeString: String to specify the data format used to encode m/z values.
//' @param int_dataTypeString: String to specify the data format used to encode intensity values.
//' @param uuid: 16 bytes long UUID.
//' @param mzArray: A matrix with the m/z values for all pixels. Each pixel corresponds to a row. If there is only one row data will be saved in continuous mode
//' @param intArray: A matrix with the intensity values for all pixels. Each pixel corresponds to a row so the number of pixels is extracted from here.
// [[Rcpp::export(name=".debug_imzMLBinWriterSequential")]]
Rcpp::DataFrame testingimzMLBinWriteSequential(const char* ibdFname, Rcpp::String mz_dataTypeString, Rcpp::String int_dataTypeString,
                                    Rcpp::String str_uuid, Rcpp::NumericMatrix mzArray, Rcpp::NumericMatrix intArray)
{
  try
  {
    
    if(mzArray.ncol() != intArray.ncol())
    {
      throw std::runtime_error("FATAL ERROR: mass channels must have the same length as intensity data");
    }
    
    double *ptr_data = new double[mzArray.ncol()];
    
    ImzMLBinWrite myWriter(ibdFname, intArray.nrow(), mz_dataTypeString, int_dataTypeString, mzArray.nrow()==1, true);
    myWriter.writeUUID(str_uuid.get_cstring());
    
    for(int i = 0; i < intArray.nrow(); i++)
    {
      Rcpp::Rcout << "Storing... "<< i << " of " << intArray.nrow() << "\n";
      
      //Store m/z data only for the first iteration if continuous
      if(!myWriter.get_continuous() || i == 0)
      {
        for(int j = 0; j < mzArray.ncol(); j++)
        {
          ptr_data[j] = mzArray(i, j);
        }
        myWriter.writeMzData(mzArray.ncol(), ptr_data);
      }  
      
      //Store intensity data
      for(int j = 0; j < intArray.ncol(); j++)
      {
        ptr_data[j] = intArray(i, j);
      }
      myWriter.writeIntData(intArray.ncol(), ptr_data);
    }
    
    myWriter.close();
    return myWriter.get_OffsetsLengths();
    
  }
  catch(std::runtime_error &e)
  {
    Rcpp::Rcout << "Catch Error: "<< e.what() << "\n";
  }
  
  return NULL;
}

//' CimzMLBinCreateNewIBD.
//' This function creates a new ibd file with the provided uuid
//' @param ibdFname: full path to the ibd file.
//' @param uuid: 16 bytes long UUID.
// [[Rcpp::export]]
void CimzMLBinCreateNewIBD(const char* ibdFname, Rcpp::String str_uuid)
{  
  try
  {
    ImzMLBinWrite myWriter(ibdFname, 0, "double", "double", true, true, false);
    myWriter.open(true); //Open here with truncation
    myWriter.writeUUID(str_uuid.get_cstring());
    myWriter.close();
  }
  catch(std::runtime_error &e)
  {
    Rcpp::Rcout << "Catch Error: "<< e.what() << "\n";
  }
}

//' CimzMLBinAppendMass.
//' This function appends a new mass axis to a given ibd file.
//' The last added offset is returned.
//' @param ibdFname: full path to the ibd file.
//' @param mz_dataTypeString:  String to specify the data format used to encode m/z values.
//' @param mzNew: The mass axis to append.
// [[Rcpp::export]]
uint64_t CimzMLBinAppendMass(const char* ibdFname, Rcpp::String mz_dataTypeString, Rcpp::NumericVector mzNew) 
{
  try
  {
    ImzMLBinWrite myWriter(ibdFname, 1, mz_dataTypeString, "double", false, true); //Assuming data in processed mode to allow appending anything
    myWriter.writeMzData(mzNew.length(), mzNew.begin());
    myWriter.close();
    return myWriter.get_mzOffset(0);
  }
  catch(std::runtime_error &e)
  {
    Rcpp::Rcout << "Catch Error: "<< e.what() << "\n";
  }
   return 0;
}

//' CimzMLBinAppendIntensity.
//' This function appends a new mass axis to a given ibd file.
//' The last added offset is returned.
//' @param ibdFname: full path to the ibd file.
//' @param int_dataTypeString:  String to specify the data format used to encode m/z values.
//' @param intNew: The mass axis to append.
// [[Rcpp::export]]
uint64_t CimzMLBinAppendIntensity(const char* ibdFname, Rcpp::String int_dataTypeString, Rcpp::NumericVector intNew) 
{
  try
  {
    ImzMLBinWrite myWriter(ibdFname, 1, "double", int_dataTypeString, false, true); //Assuming data in processed mode to allow appending anything
    myWriter.writeIntData(intNew.length(), intNew.begin());
    myWriter.close();
    return myWriter.get_intOffset(0);
  }
  catch(std::runtime_error &e)
  {
    Rcpp::Rcout << "Catch Error: "<< e.what() << "\n";
  }
  return 0;
}

//' A method to use the imzMLwriter in modify mode to allow direct modification of mass axes for the calibration
//' This function modifies data of an ibd file with the following params
//' @param ibdFname: full path to the ibd file.
//' @param NPixels: Total number of pixels in the image.
//' @param mz_dataTypeString: String to specify the data format used to encode m/z values.
//' @param int_dataTypeString: String to specify the data format used to encode intensity values.
//' @param continuous: true if imzML data is in continuous mode
//' @param mzNew: A vector with the m/z values. Must be the same length as the original imzML mass target massa axis.
//' @param mzOffset: offset in the ibd file of the target mass axis.
// [[Rcpp::export]]
void CimzMLBinWriteModifyMass(const char* ibdFname, unsigned int NPixels, Rcpp::String mz_dataTypeString, Rcpp::String int_dataTypeString, bool continuous,
                                           Rcpp::NumericVector mzNew, uint64_t mzOffset)
{
  try
  {
    ImzMLBinWrite myWriter(ibdFname, NPixels, mz_dataTypeString, int_dataTypeString, continuous, false);
    myWriter.writeMzData(mzOffset, mzNew.length(), mzNew.begin() );
    myWriter.close();
  }
  catch(std::runtime_error &e)
  {
    Rcpp::Rcout << "Catch Error: "<< e.what() << "\n";
  }
}

//' CimzMLBinReadMass.
//' 
//' Reads a single mass axis from the imzML file.
//' 
//' @param ibdFname: full path to the ibd file.
//' @param NPixels: Total number of pixels in the image.
//' @param N: number of elemetns (or data point to read).
//' @param offset: offset in bytes at which the reading operation is started.
//' @param continuous: true if imzML data is in continuous mode
// [[Rcpp::export]]
Rcpp::NumericVector CimzMLBinReadMass(const char* ibdFname, unsigned int NPixels, unsigned int N, uint64_t offset, Rcpp::String dataTypeString, bool continuous)
{
  return imzMLBinReadGeneric(ibdFname, NPixels, N, offset, dataTypeString, true, continuous);
}

//' CimzMLBinReadIntensity.
//' 
//' Reads a single mass axis from the imzML file.
//' 
//' @param ibdFname: full path to the ibd file.
//' @param NPixels: Total number of pixels in the image.
//' @param N: number of elemetns (or data point to read).
//' @param offset: offset in bytes at which the reading operation is started.
//' @param continuous: true if imzML data is in continuous mode
// [[Rcpp::export]]
Rcpp::NumericVector CimzMLBinReadIntensity(const char* ibdFname, unsigned int NPixels, unsigned int N, uint64_t offset, Rcpp::String dataTypeString, bool continuous)
{
  return imzMLBinReadGeneric(ibdFname, NPixels, N, offset, dataTypeString, false, continuous);
}

//' Method to read a peak list from an imzML file. Processed mode is assumed.
//' testingimzMLBinRead
//' @param ibdFname: full path to the ibd file.
//' @param imzML_peakList_descriptor: imzML file description as it is returned by the CimzMLParse() function.
//' @param PixelID: the pixel ID to read a peak list.
// [[Rcpp::export]]
Rcpp::List CimzMLReadPeakList(const char* ibdFname, Rcpp::List imzML_peakList_descriptor, unsigned int PixelID)
{
  if (Rcpp::as<bool>(imzML_peakList_descriptor["continuous_mode"]))
  {
    Rcpp::stop("ERROR: imzML file is in continuous mode so it cannot be readed as  peak list.\n");  
  }

  //Extract the RunData Matrix 
  Rcpp::DataFrame runMat = Rcpp::as<Rcpp::DataFrame>(imzML_peakList_descriptor["run_data"]);
  
  unsigned int NPixels = runMat.nrows();
  Rcpp::String Str_mzType = Rcpp::as< Rcpp::String>(imzML_peakList_descriptor["mz_dataType"]); 
  Rcpp::String Str_intType = Rcpp::as< Rcpp::String>(imzML_peakList_descriptor["int_dataType"]); 
  Rcpp::NumericVector mzLength_vector = runMat["mzLength"];
  Rcpp::NumericVector mzOffset_vector = runMat["mzOffset"]; 
  Rcpp::NumericVector intLength_vector = runMat["intLength"]; 
  Rcpp::NumericVector intOffset_vector = runMat["intOffset"]; 
  bool bPeakListrMSIformat = Rcpp::as<bool>(imzML_peakList_descriptor["rMSIpeakList"]); 
  
  PeakPicking::Peaks *mpeaks;
  
  try
  {
    ImzMLBinRead myReader(ibdFname, NPixels, Str_mzType, Str_intType, false, true, bPeakListrMSIformat);
    myReader.set_mzLength(&mzLength_vector);
    myReader.set_mzOffset(&mzOffset_vector);
    myReader.set_intLength(&intLength_vector);
    myReader.set_intOffset(&intOffset_vector);
    mpeaks = myReader.ReadPeakList(PixelID);
    myReader.close();
  }
  catch(std::runtime_error &e)
  {
    delete mpeaks;
    Rcpp::stop(e.what());
  }
  
  Rcpp::NumericVector vmass(mpeaks->mass.size());
  Rcpp::NumericVector vintensity(mpeaks->intensity.size());
  Rcpp::NumericVector varea(mpeaks->area.size());
  Rcpp::NumericVector vsnr(mpeaks->SNR.size());
  Rcpp::NumericVector vbinsize(mpeaks->binSize.size());
  
  memcpy(vmass.begin(), mpeaks->mass.data(), vmass.length() * sizeof(double) );
  memcpy(vintensity.begin(), mpeaks->intensity.data(), vintensity.length() * sizeof(double));
  memcpy(varea.begin(), mpeaks->area.data(), varea.length() * sizeof(double) );
  memcpy(vsnr.begin(), mpeaks->SNR.data(), vsnr.length() * sizeof(double) );
  memcpy(vbinsize.begin(), mpeaks->binSize.data(), vbinsize.length() * sizeof(double) );
  
  delete mpeaks;
  
  if(bPeakListrMSIformat)
  {
    return Rcpp::List::create( Rcpp::Named("ID") = PixelID,
                             Rcpp::Named("mass") = vmass,
                             Rcpp::Named("intensity") = vintensity,
                             Rcpp::Named("area") = varea,
                             Rcpp::Named("SNR") = vsnr,
                             Rcpp::Named("binSize") = vbinsize);
  }
  else
  {
    return Rcpp::List::create( Rcpp::Named("ID") = PixelID,
                               Rcpp::Named("mass") = vmass,
                               Rcpp::Named("intensity") = vintensity);
  }
}

//' Method to overwrite the UUID of an imzML ibd file.
//' testingimzMLBinRead
//' @param ibdFname: full path to the ibd file.
//' @param newUUID: the new uuid as a string.
// [[Rcpp::export]]
void overwriteIbdUUid(const char* ibdFname, Rcpp::String newUUID)
{
  try
  {
    ImzMLBinWrite myWriter(ibdFname, 1, "double", "double", true, false);
    myWriter.overwriteUUID(newUUID.get_cstring());
    myWriter.close();
  }
  catch(std::runtime_error &e)
  {
    Rcpp::Rcout << "Catch Error: "<< e.what() << "\n";
  }
}

#define SPECTRA_BUFFER_MB 1024 //I think 1024 MB of RAM is a good limit for spectra loading

//' Cload_imzMLSpectra
//' Load spectra into a Matrix object interpolating to the common mass axis when necessary.
//' @param rMSIobj: an rMSI object prefilled with a parsed imzML.
//' @param pixelIDs: pixel ID's of the spectra to load in C-style indexing (starting at 0).
//' @param commonMassAxis: a common mass axis that may be different than the mass axis in the rMSI object.
//' @param number_of_threads: number of thread to use during interpolation
// [[Rcpp::export]]
Rcpp::NumericMatrix Cload_imzMLSpectra(Rcpp::List rMSIobj, Rcpp::IntegerVector pixelIDs, Rcpp::NumericVector commonMassAxis, unsigned int number_of_threads)
{
  Rcpp::NumericMatrix m_spc;
  double *buffer;
  
  try
  {
    //Allocate the spectra reading buffer
    buffer = new double[commonMassAxis.length() * pixelIDs.length()];
    
    //Allocate the output matrix
    if( ((pixelIDs.length() * commonMassAxis.length() * sizeof(double))/ (1024 * 1024 )) > SPECTRA_BUFFER_MB )
    {
      throw std::runtime_error("Error in Cload_imzMLSpectra(): loading data required too much memory.");
    }
    m_spc = Rcpp::NumericMatrix(pixelIDs.length(), commonMassAxis.length());
    
    //Set the imzML reader
    Rcpp::List data = rMSIobj["data"];
    Rcpp::List imzML = data["imzML"];
    Rcpp::DataFrame imzMLrun = Rcpp::as<Rcpp::DataFrame>(imzML["run"]);
    std::string sFilePath = Rcpp::as<std::string>(data["path"]);
    std::string sFnameImzML = Rcpp::as<std::string>(imzML["file"]);
    sFnameImzML= sFilePath + "/" + sFnameImzML + ".ibd";
    ImzMLBinRead imzMLReader(sFnameImzML.c_str(), 
                             imzMLrun.nrows(), 
                             Rcpp::as<Rcpp::String>(imzML["mz_dataType"]),
                             Rcpp::as<Rcpp::String>(imzML["int_dataType"]) ,
                             Rcpp::as<bool>(imzML["continuous_mode"]));
    
    imzMLReader.setCommonMassAxis(commonMassAxis.length(), commonMassAxis.begin());
    
    Rcpp::NumericVector imzML_mzLength = imzMLrun["mzLength"];
    Rcpp::NumericVector imzML_mzOffsets = imzMLrun["mzOffset"];
    Rcpp::NumericVector imzML_intLength = imzMLrun["intLength"];
    Rcpp::NumericVector imzML_intOffsets = imzMLrun["intOffset"];
    imzMLReader.set_mzLength(&imzML_mzLength);  
    imzMLReader.set_mzOffset(&imzML_mzOffsets);
    imzMLReader.set_intLength(&imzML_intLength);
    imzMLReader.set_intOffset(&imzML_intOffsets);
    
    //Load spectra and copy to the output array
    imzMLReader.ReadSpectra(pixelIDs.length(), (unsigned int *) pixelIDs.begin(), 0, commonMassAxis.length(), buffer, number_of_threads); 
    for(int i=0; i < pixelIDs.length(); i++)
    {
      for(int j = 0; j < commonMassAxis.length(); j++)
      {
        m_spc(i,j) = buffer[j + i*commonMassAxis.length()];
      }
    }
    
    delete[] buffer;
  }
  catch(std::runtime_error &e)
  {
    delete[] buffer;
    Rcpp::stop(e.what());
  }
  
  return m_spc;
}

