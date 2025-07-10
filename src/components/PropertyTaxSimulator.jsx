import React, { useState, useCallback, useMemo } from 'react';
import { Upload, FileText, CheckCircle, XCircle, AlertCircle, Download, ChevronRight } from 'lucide-react';
import Papa from 'papaparse';

const PropertyTaxSimulator = () => {
  // State management
  const [files, setFiles] = useState({
    property: null,
    payment: null,
    business: null
  });
  
  const [data, setData] = useState({
    property: null,
    payment: null,
    business: null
  });
  
  const [columnMappings, setColumnMappings] = useState({
    // Property data mappings
    id_property: '',
    latitude: '',
    longitude: '',
    property_area: '',
    property_type: '',
    structure_type: [],
    property_features: [],
    // Payment data mappings
    payment_id_property: '',
    made_payment: '',
    // Business data mappings
    business_id_property: '',
    business_name: '',
    business_area: '',
    business_category: ''
  });
  
  const [processedData, setProcessedData] = useState(null);
  const [activeTab, setActiveTab] = useState('upload');
  const [validationResults, setValidationResults] = useState([]);

  // Navigation helper
  const tabs = ['upload', 'mapping', 'validation', 'processing'];
  const goToNextTab = () => {
    const currentIndex = tabs.indexOf(activeTab);
    if (currentIndex < tabs.length - 1) {
      setActiveTab(tabs[currentIndex + 1]);
    }
  };

  // File upload handler
  const handleFileUpload = useCallback((event, fileType) => {
    const file = event.target.files[0];
    if (!file) return;
    
    setFiles(prev => ({ ...prev, [fileType]: file }));
    
    Papa.parse(file, {
      complete: (result) => {
        setData(prev => ({ ...prev, [fileType]: result.data }));
      },
      header: true,
      skipEmptyLines: true,
      dynamicTyping: true
    });
  }, []);

  // Column mapping handler
  const handleColumnMapping = useCallback((field, value) => {
    setColumnMappings(prev => ({ ...prev, [field]: value }));
  }, []);

  // Get available columns for a dataset
  const getColumns = useCallback((dataType) => {
    if (!data[dataType] || data[dataType].length === 0) return [];
    return Object.keys(data[dataType][0]);
  }, [data]);

  // Validation function
  const validateData = useCallback(() => {
    const results = [];
    
    // Check file uploads
    if (data.property) {
      results.push({ status: 'success', message: `Property data loaded: ${data.property.length} records` });
    } else {
      results.push({ status: 'error', message: 'Property data not loaded' });
    }
    
    if (data.payment) {
      results.push({ status: 'success', message: `Payment data loaded: ${data.payment.length} records` });
    } else {
      results.push({ status: 'error', message: 'Payment data not loaded' });
    }
    
    if (data.business) {
      results.push({ status: 'success', message: `Business data loaded: ${data.business.length} records` });
    } else {
      results.push({ status: 'warning', message: 'Business data not loaded (optional)' });
    }
    
    // Check required mappings
    const requiredMappings = ['id_property', 'latitude', 'longitude', 'property_area', 'property_type'];
    requiredMappings.forEach(mapping => {
      if (columnMappings[mapping]) {
        results.push({ status: 'success', message: `${mapping} mapped correctly` });
      } else {
        results.push({ status: 'error', message: `${mapping} not mapped` });
      }
    });
    
    // Check ID consistency
    if (data.property && data.payment && columnMappings.id_property && columnMappings.payment_id_property) {
      const propertyIds = new Set(data.property.map(row => row[columnMappings.id_property]));
      const paymentIds = new Set(data.payment.map(row => row[columnMappings.payment_id_property]));
      const commonIds = [...propertyIds].filter(id => paymentIds.has(id));
      
      results.push({ 
        status: 'info', 
        message: `Found ${commonIds.length} properties with payment records` 
      });
    }
    
    setValidationResults(results);
  }, [data, columnMappings]);

  // Process data with one-hot encoding
  const processData = useCallback(() => {
    if (!data.property || !columnMappings.id_property) {
      alert('Please upload property data and map required columns first!');
      return;
    }
    
    let processed = [...data.property];
    const encodingSummary = {};
    
    // Get all categorical columns
    const categoricalColumns = [
      columnMappings.property_type,
      ...columnMappings.structure_type,
      ...columnMappings.property_features
    ].filter(Boolean);
    
    // Perform one-hot encoding
    categoricalColumns.forEach(col => {
      const uniqueValues = [...new Set(data.property.map(row => row[col]).filter(val => val !== null && val !== undefined))];
      encodingSummary[col] = uniqueValues;
      
      // Add new columns for each unique value
      uniqueValues.forEach(value => {
        const newColName = `${col.toLowerCase().replace(/ /g, '_')}_${String(value).toLowerCase().replace(/ /g, '_')}`;
        processed = processed.map(row => ({
          ...row,
          [newColName]: row[col] === value ? 1 : 0
        }));
      });
    });
    
    // Add payment status if available
    if (data.payment && columnMappings.payment_id_property && columnMappings.made_payment) {
      const paymentMap = {};
      data.payment.forEach(row => {
        const id = row[columnMappings.payment_id_property];
        if (!paymentMap[id]) {
          paymentMap[id] = row[columnMappings.made_payment];
        }
      });
      
      processed = processed.map(row => ({
        ...row,
        made_payment_2024: paymentMap[row[columnMappings.id_property]] || false
      }));
    }
    
    setProcessedData({ data: processed, encodingSummary });
  }, [data, columnMappings]);

  // Export processed data
  const exportData = useCallback(() => {
    if (!processedData) return;
    
    const csv = Papa.unparse(processedData.data);
    const blob = new Blob([csv], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'processed_property_data.csv';
    a.click();
  }, [processedData]);

  // Export column mappings
  const exportMappings = useCallback(() => {
    const blob = new Blob([JSON.stringify(columnMappings, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'column_mappings.json';
    a.click();
  }, [columnMappings]);

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Header */}
      <div className="bg-white shadow-sm border-b">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="py-6">
            <h1 className="text-3xl font-bold text-gray-900">
              🏢 Property Tax Simulator - Module 1: Data Input
            </h1>
            <p className="mt-2 text-gray-600">
              Upload your city's data files and map columns to create a standardized dataset for analysis
            </p>
          </div>
        </div>
      </div>

      {/* Navigation Tabs */}
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 mt-6">
        <div className="border-b border-gray-200">
          <nav className="-mb-px flex space-x-8">
            {['upload', 'mapping', 'validation', 'processing'].map((tab) => (
              <button
                key={tab}
                onClick={() => setActiveTab(tab)}
                className={`
                  py-2 px-1 border-b-2 font-medium text-sm capitalize
                  ${activeTab === tab 
                    ? 'border-blue-500 text-blue-600' 
                    : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'}
                `}
              >
                {tab === 'upload' && '📁 File Upload'}
                {tab === 'mapping' && '🔗 Column Mapping'}
                {tab === 'validation' && '✅ Data Validation'}
                {tab === 'processing' && '⚙️ Processing'}
              </button>
            ))}
          </nav>
        </div>
      </div>

      {/* Content Area */}
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* File Upload Tab */}
        {activeTab === 'upload' && (
          <>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
              {/* Property Data Upload */}
              <div className="bg-white rounded-lg shadow p-6">
                <h3 className="text-lg font-semibold mb-4">Property Data</h3>
                <p className="text-sm text-gray-600 mb-4">
                  Database of all taxable city properties with features that impact value + location
                </p>
                <label className="block">
                  <input
                    type="file"
                    accept=".csv"
                    onChange={(e) => handleFileUpload(e, 'property')}
                    className="hidden"
                  />
                  <div className="border-2 border-dashed border-gray-300 rounded-lg p-6 cursor-pointer hover:border-blue-400 transition-colors">
                    <Upload className="mx-auto h-12 w-12 text-gray-400" />
                    <p className="mt-2 text-sm text-gray-600 text-center">
                      Click to upload property_data.csv
                    </p>
                  </div>
                </label>
                {data.property && (
                  <div className="mt-4 p-3 bg-green-50 rounded flex items-center">
                    <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                    <span className="text-sm text-green-700">
                      Loaded {data.property.length} properties
                    </span>
                  </div>
                )}
              </div>

              {/* Payment Data Upload */}
              <div className="bg-white rounded-lg shadow p-6">
                <h3 className="text-lg font-semibold mb-4">Payment Data</h3>
                <p className="text-sm text-gray-600 mb-4">
                  Database of properties that made a payment in 2024
                </p>
                <label className="block">
                  <input
                    type="file"
                    accept=".csv"
                    onChange={(e) => handleFileUpload(e, 'payment')}
                    className="hidden"
                  />
                  <div className="border-2 border-dashed border-gray-300 rounded-lg p-6 cursor-pointer hover:border-blue-400 transition-colors">
                    <Upload className="mx-auto h-12 w-12 text-gray-400" />
                    <p className="mt-2 text-sm text-gray-600 text-center">
                      Click to upload made_payment.csv
                    </p>
                  </div>
                </label>
                {data.payment && (
                  <div className="mt-4 p-3 bg-green-50 rounded flex items-center">
                    <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                    <span className="text-sm text-green-700">
                      Loaded {data.payment.length} payment records
                    </span>
                  </div>
                )}
              </div>

              {/* Business Data Upload */}
              <div className="bg-white rounded-lg shadow p-6">
                <h3 className="text-lg font-semibold mb-4">Business Data</h3>
                <p className="text-sm text-gray-600 mb-4">
                  Database of businesses with ID column to match properties
                </p>
                <label className="block">
                  <input
                    type="file"
                    accept=".csv"
                    onChange={(e) => handleFileUpload(e, 'business')}
                    className="hidden"
                  />
                  <div className="border-2 border-dashed border-gray-300 rounded-lg p-6 cursor-pointer hover:border-blue-400 transition-colors">
                    <Upload className="mx-auto h-12 w-12 text-gray-400" />
                    <p className="mt-2 text-sm text-gray-600 text-center">
                      Click to upload businesses.csv
                    </p>
                  </div>
                </label>
                {data.business && (
                  <div className="mt-4 p-3 bg-green-50 rounded flex items-center">
                    <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                    <span className="text-sm text-green-700">
                      Loaded {data.business.length} business records
                    </span>
                  </div>
                )}
              </div>
            </div>
            
            {/* Next Button */}
            <div className="mt-8 flex justify-end">
              <button
                onClick={goToNextTab}
                className="px-6 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors flex items-center"
              >
                Next
                <ChevronRight className="ml-2 h-4 w-4" />
              </button>
            </div>
          </>
        )}

        {/* Column Mapping Tab */}
        {activeTab === 'mapping' && (
          <>
            <div className="space-y-8">
              {/* Property Data Mapping */}
              {data.property && (
                <div className="bg-white rounded-lg shadow p-6">
                  <h3 className="text-xl font-semibold mb-4">Property Data Column Mapping</h3>
                  <p className="text-gray-600 mb-6">Indicate which of the columns from your uploaded files correspond with the following data categories:</p>
                  
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                    {/* Single Column Mappings */}
                    <div>
                      <div className="space-y-3">
                        {[
                          { field: 'id_property', label: 'ID Property' },
                          { field: 'latitude', label: 'Latitude' },
                          { field: 'longitude', label: 'Longitude' },
                          { field: 'property_area', label: 'Property Area' },
                          { field: 'property_type', label: 'Property Type' }
                        ].map(({ field, label }) => (
                          <div key={field}>
                            <label className="block text-sm font-medium text-gray-700 mb-1">
                              {label}
                            </label>
                            <select
                              value={columnMappings[field]}
                              onChange={(e) => handleColumnMapping(field, e.target.value)}
                              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
                            >
                              <option value="">Select column...</option>
                              {getColumns('property').map(col => (
                                <option key={col} value={col}>{col}</option>
                              ))}
                            </select>
                          </div>
                        ))}
                      </div>
                    </div>

                    {/* Multiple Column Mappings */}
                    <div>
                      <div className="space-y-3">
                        {[
                          { field: 'structure_type', label: 'Structure Type Columns' },
                          { field: 'property_features', label: 'Property Feature Columns' }
                        ].map(({ field, label }) => (
                          <div key={field}>
                            <label className="block text-sm font-medium text-gray-700 mb-1">
                              {label} <span className="text-gray-500 font-normal">(select all that apply)</span>
                            </label>
                            <div className="space-y-1 max-h-32 overflow-y-auto border border-gray-300 rounded-md p-2">
                              {getColumns('property').map(col => (
                                <label key={col} className="flex items-center space-x-2">
                                  <input
                                    type="checkbox"
                                    checked={columnMappings[field].includes(col)}
                                    onChange={(e) => {
                                      if (e.target.checked) {
                                        handleColumnMapping(field, [...columnMappings[field], col]);
                                      } else {
                                        handleColumnMapping(field, columnMappings[field].filter(c => c !== col));
                                      }
                                    }}
                                    className="rounded text-blue-600"
                                  />
                                  <span className="text-sm">{col}</span>
                                </label>
                              ))}
                            </div>
                          </div>
                        ))}
                      </div>
                    </div>
                  </div>
                </div>
              )}

              {/* Payment Data Mapping */}
              {data.payment && (
                <div className="bg-white rounded-lg shadow p-6">
                  <h3 className="text-xl font-semibold mb-4">Payment Data Column Mapping</h3>
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                    {[
                      { field: 'payment_id_property', label: 'ID Property' },
                      { field: 'made_payment', label: 'Made Payment' }
                    ].map(({ field, label }) => (
                      <div key={field}>
                        <label className="block text-sm font-medium text-gray-700 mb-1">
                          {label}
                        </label>
                        <select
                          value={columnMappings[field]}
                          onChange={(e) => handleColumnMapping(field, e.target.value)}
                          className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
                        >
                          <option value="">Select column...</option>
                          {getColumns('payment').map(col => (
                            <option key={col} value={col}>{col}</option>
                          ))}
                        </select>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* Business Data Mapping */}
              {data.business && (
                <div className="bg-white rounded-lg shadow p-6">
                  <h3 className="text-xl font-semibold mb-4">Business Data Column Mapping</h3>
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                    {[
                      { field: 'business_id_property', label: 'ID Property' },
                      { field: 'business_name', label: 'Business Name' },
                      { field: 'business_area', label: 'Business Area' },
                      { field: 'business_category', label: 'Business Category' }
                    ].map(({ field, label }) => (
                      <div key={field}>
                        <label className="block text-sm font-medium text-gray-700 mb-1">
                          {label}
                        </label>
                        <select
                          value={columnMappings[field]}
                          onChange={(e) => handleColumnMapping(field, e.target.value)}
                          className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
                        >
                          <option value="">Select column...</option>
                          {getColumns('business').map(col => (
                            <option key={col} value={col}>{col}</option>
                          ))}
                        </select>
                      </div>
                    ))}
                  </div>
                </div>
              )}
            </div>
            
            {/* Next Button */}
            <div className="mt-8 flex justify-end">
              <button
                onClick={goToNextTab}
                className="px-6 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors flex items-center"
              >
                Next
                <ChevronRight className="ml-2 h-4 w-4" />
              </button>
            </div>
          </>
        )}

        {/* Validation Tab */}
        {activeTab === 'validation' && (
          <>
            <div className="bg-white rounded-lg shadow p-6">
              <h3 className="text-xl font-semibold mb-4">Data Validation</h3>
              
              <button
                onClick={validateData}
                className="mb-6 px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors"
              >
                Run Validation Checks
              </button>

              {validationResults.length > 0 && (
                <div className="space-y-2">
                  {validationResults.map((result, index) => (
                    <div key={index} className="flex items-center space-x-2">
                      {result.status === 'success' && <CheckCircle className="h-5 w-5 text-green-500" />}
                      {result.status === 'error' && <XCircle className="h-5 w-5 text-red-500" />}
                      {result.status === 'warning' && <AlertCircle className="h-5 w-5 text-yellow-500" />}
                      {result.status === 'info' && <AlertCircle className="h-5 w-5 text-blue-500" />}
                      <span className={`text-sm ${
                        result.status === 'success' ? 'text-green-700' :
                        result.status === 'error' ? 'text-red-700' :
                        result.status === 'warning' ? 'text-yellow-700' :
                        'text-blue-700'
                      }`}>
                        {result.message}
                      </span>
                    </div>
                  ))}
                </div>
              )}
            </div>
            
            {/* Next Button */}
            <div className="mt-8 flex justify-end">
              <button
                onClick={goToNextTab}
                className="px-6 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors flex items-center"
              >
                Next
                <ChevronRight className="ml-2 h-4 w-4" />
              </button>
            </div>
          </>
        )}

        {/* Processing Tab */}
        {activeTab === 'processing' && (
          <div className="space-y-6">
            <div className="bg-white rounded-lg shadow p-6">
              <h3 className="text-xl font-semibold mb-4">Data Processing & Feature Engineering</h3>
              
              <button
                onClick={processData}
                className="mb-6 px-6 py-3 bg-green-600 text-white rounded-md hover:bg-green-700 transition-colors flex items-center"
              >
                <ChevronRight className="mr-2 h-5 w-5" />
                Process Data
              </button>

              {processedData && (
                <>
                  <div className="mb-6 p-4 bg-green-50 rounded-lg">
                    <p className="text-green-700">
                      ✅ Data processing complete! Processed dataset has {processedData.data.length} rows 
                      and {Object.keys(processedData.data[0]).length} columns
                    </p>
                  </div>

                  {/* Encoding Summary */}
                  <div className="mb-6">
                    <h4 className="font-medium mb-3">Encoding Summary</h4>
                    <div className="space-y-2">
                      {Object.entries(processedData.encodingSummary).map(([col, values]) => (
                        <div key={col} className="text-sm">
                          <span className="font-medium">{col}:</span> {values.length} unique values
                          <div className="text-xs text-gray-600 mt-1">
                            {values.slice(0, 5).join(', ')}
                            {values.length > 5 && ` ... and ${values.length - 5} more`}
                          </div>
                        </div>
                      ))}
                    </div>
                  </div>

                  {/* Export Buttons */}
                  <div className="flex space-x-4">
                    <button
                      onClick={exportData}
                      className="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors flex items-center"
                    >
                      <Download className="mr-2 h-4 w-4" />
                      Download Processed Data
                    </button>
                    <button
                      onClick={exportMappings}
                      className="px-4 py-2 bg-gray-600 text-white rounded-md hover:bg-gray-700 transition-colors flex items-center"
                    >
                      <Download className="mr-2 h-4 w-4" />
                      Download Column Mappings
                    </button>
                  </div>
                </>
              )}
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default PropertyTaxSimulator;