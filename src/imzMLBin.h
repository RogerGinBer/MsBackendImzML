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


#ifndef IMZML_BIN_H
#define IMZML_BIN_H

#include <fstream>
#include <vector>
#include <Rcpp.h>
#include "encoder_settings.h"

typedef struct
{
  int pixelID;
  unsigned int last_offset; //The last position in the ibd file when the spectrum was read
  std::vector<double> imzMLmass; 
  std::vector<double> imzMLintensity; 
}imzMLSpectrum; //If data is in continuous mode the std::vectors will be empty

class ImzMLBin
{
  public:
    enum imzMLDataType
    {
      int32, //32 bits integer  (int)
      int64, //64 bits interger (long)
      float32, //32 bits float  (float)
      float64  //64 bits float  (double)
    } ;
    
    enum Mode { Read, SequentialWriteFile, ModifyFile }; //The mode must be spcified in the constructor
    
    ImzMLBin(const char* ibd_fname, unsigned int num_of_pixels, Rcpp::String Str_mzType, Rcpp::String Str_intType, bool continuous, Mode mode);
    ~ImzMLBin();
    
    const char* getIbdFilePath();
    bool get_continuous();
    
    unsigned int get_number_of_pixels();
    
    //Get the number of bytes used for encoding
    unsigned int get_mzEncodingBytes();
    unsigned int get_intEncodingBytes();
    
    //Close the file connection
    void close();
    
  protected:
    Mode fileMode; //Define the mode used to acces the binary file
    Rcpp::String ibdFname;
    std::fstream ibdFile; 
    unsigned int Npixels; //Total number of pixels in the image;
    unsigned int mzDataPointBytes; //Number of bytes used to encode a mass channel 
    unsigned int intDataPointBytes; //Number of bytes used to encode an intensity data point
    imzMLDataType mzDataType, intDataType;
    bool bContinuous;
    
    //Get the imzMLDataType from a string
    imzMLDataType string2imzMLDatatype(Rcpp::String data_type);
    
    template<typename T> 
    void convertBytes2Double(char* inBytes, double* outPtr, unsigned int N);
    
    template<typename T> 
    void convertDouble2Bytes(double* inPtr, char* outBytes, unsigned int N);
};

class ImzMLBinRead : public ImzMLBin
{
  public: 
    ImzMLBinRead(const char* ibd_fname, unsigned int num_of_pixels, Rcpp::String Str_mzType, Rcpp::String Str_intType, bool continuous, 
                 bool openIbd = true, bool peakListrMSIformat = false);
    ~ImzMLBinRead();
    
    //Open the ibd file in reading mode
    void open();
    
    //Read N elements from the ibd file and decode them as intensity data.
    //offset: offset in bytes at which the reading operation is started.
    //N: number of elements to read from the ibd file (N is elements, not bytes!)
    //ptr: Data will be stored at the ptr pointer
    void readIntData(std::streampos offset, unsigned int N, double* ptr );
    

  private:
    //Read N elements from the ibd file and decode them.
    //offset: offset in bytes at which the reading operation is started. If set to -1 no seek operation is used.
    //N: number of elements to read from the ibd file (N is elements, not bytes!)
    //ptr: Data will be stored at the ptr pointer
    //dataPointBytes: number of bytes used to encode a single data point.
    //dataType: data type used for the encoding.
    void readDataCommon(std::streampos offset, unsigned int N, double* ptr, unsigned int dataPointBytes, imzMLDataType dataType);
    
    std::vector<unsigned int>  pixels_read_offsets; //A vector to store all the previous offset readed to allow a faster acces in processed mode;
    
};


#endif
