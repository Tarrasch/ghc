import GHC.ExecutionStack

main = currentExecutionStack >>= printExecutionStack  
