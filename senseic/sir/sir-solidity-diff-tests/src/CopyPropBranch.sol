// SPDX-License-Identifier: MIT
pragma solidity =0.8.30;

contract CopyPropBranch {
    fallback() external payable {
        assembly ("memory-safe") {
            let x := calldataload(0)
            let y := calldataload(32)
            let result := add(x, y)
            if result {
                mstore(0, result)
                return(0, 32)
            }
            mstore(0, 0)
            return(0, 32)
        }
    }
}
