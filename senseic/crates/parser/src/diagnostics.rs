use crate::lexer::SourceSpan;
use allocator_api2::vec::Vec;
use bumpalo::Bump;

#[derive(Debug, Clone)]
pub struct Diagnostic<'ast> {
    pub span: SourceSpan,
    pub message: &'ast str,
    pub notes: Vec<DiagnosticNote<'ast>, &'ast Bump>,
}

#[derive(Debug, Clone)]
pub struct DiagnosticNote<'ast> {
    pub span: Option<SourceSpan>,
    pub message: &'ast str,
}

pub struct DiagnosticsContext<'ast> {
    arena: &'ast Bump,
    errors: Vec<Diagnostic<'ast>, &'ast Bump>,
}

impl<'ast> DiagnosticsContext<'ast> {
    pub fn new(arena: &'ast Bump) -> Self {
        Self { arena, errors: Vec::new_in(arena) }
    }

    pub fn report(&mut self, span: SourceSpan, message: impl AsRef<str>) {
        let message = self.arena.alloc_str(message.as_ref());
        self.errors.push(Diagnostic { span, message, notes: Vec::new_in(self.arena) });
    }

    pub fn report_with_note(
        &mut self,
        span: SourceSpan,
        message: impl AsRef<str>,
        note: impl AsRef<str>,
    ) {
        let message = self.arena.alloc_str(message.as_ref());
        let note_message = self.arena.alloc_str(note.as_ref());
        let mut notes = Vec::new_in(self.arena);
        notes.push(DiagnosticNote { span: None, message: note_message });
        self.errors.push(Diagnostic { span, message, notes });
    }

    pub fn report_with_span_note(
        &mut self,
        span: SourceSpan,
        message: impl AsRef<str>,
        note_span: SourceSpan,
        note: impl AsRef<str>,
    ) {
        let message = self.arena.alloc_str(message.as_ref());
        let note_message = self.arena.alloc_str(note.as_ref());
        let mut notes = Vec::new_in(self.arena);
        notes.push(DiagnosticNote { span: Some(note_span), message: note_message });
        self.errors.push(Diagnostic { span, message, notes });
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    pub fn errors(&self) -> &[Diagnostic<'ast>] {
        &self.errors
    }

    pub fn into_errors(self) -> Vec<Diagnostic<'ast>, &'ast Bump> {
        self.errors
    }
}
