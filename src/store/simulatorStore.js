import { create } from 'zustand'

const useSimulatorStore = create((set) => ({
  // Module 1 state
  processedData: null,
  columnMappings: {},
  
  // Module 2 state
  coefficients: {},
  taxRates: {},
  
  // Actions
  setProcessedData: (data) => set({ processedData: data }),
  updateCoefficients: (coeffs) => set({ coefficients: coeffs }),
  
  // Persist to localStorage
  loadState: () => {
    const saved = localStorage.getItem('simulator-state')
    if (saved) set(JSON.parse(saved))
  },
  
  saveState: () => {
    const state = useSimulatorStore.getState()
    localStorage.setItem('simulator-state', JSON.stringify(state))
  }
}))

export default useSimulatorStore