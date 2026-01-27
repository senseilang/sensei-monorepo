mod copy_propagation;

use sir_data::EthIRProgram;

pub enum Optimization {
    CopyPropagation,
}

pub trait CompilerOptimization {
    fn apply(&mut self, optimization: Optimization);
}

impl CompilerOptimization for EthIRProgram {
    fn apply(&mut self, optimization: Optimization) {
        match optimization {
            Optimization::CopyPropagation => copy_propagation::run(self),
        }
    }
}
