use dashmap::DashMap;
use rize_syntax::{lex, SyntaxKind};
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    pub documents: DashMap<String, String>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_owned()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    ..Default::default()
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_owned()],
                    work_done_progress_options: Default::default(),
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: Default::default(),
                            legend: SemanticTokensLegend {
                                token_types: vec![
                                    SemanticTokenType::KEYWORD,
                                    SemanticTokenType::VARIABLE,
                                    SemanticTokenType::TYPE,
                                    SemanticTokenType::STRING,
                                ],
                                token_modifiers: vec![],
                            },
                            range: None,
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        self.client
            .log_message(MessageType::INFO, "command executed!")
            .await;

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let text = params.text_document.text;
        self.documents.insert(uri.to_string(), text);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let changes = &params.content_changes;
        if let Some(change) = changes.first() {
            self.documents.insert(uri.to_string(), change.text.clone());
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        if let Some(text) = &params.text {
            self.documents.insert(uri, text.clone());
        }
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_owned(), "Some detail".to_owned()),
            CompletionItem::new_simple("Bye".to_owned(), "More detail".to_owned()),
        ])))
    }

    
async fn semantic_tokens_full(
    &self,
    params: SemanticTokensParams,
) -> Result<Option<SemanticTokensResult>> {
    let uri = params.text_document.uri.to_string();
    let doc = self.documents.get(&uri).unwrap();
    let text = &doc;

    let tokens = lex(text); // Token { kind, text }

    let mut semantic_tokens = vec![];
    let mut byte_offset = 0;
    let mut prev_line = 0;
    let mut prev_start_char = 0;

    for token in tokens {
        let token_start = byte_offset;
        let token_len = token.text.chars().count();
        byte_offset += token_len;

        // Map byte offset to line and character position
        let prefix = &text[..token_start];
        let token_line = prefix.lines().count() - 1;
        let token_col = prefix.lines().last().map_or(0, |l| l.len());

        // Skip unknown tokens
        let kind = match token.kind {
            SyntaxKind::Let => SemanticTokenType::KEYWORD,
            SyntaxKind::Ident => SemanticTokenType::VARIABLE,
            SyntaxKind::Type => SemanticTokenType::TYPE,
            SyntaxKind::StringLiteral => SemanticTokenType::STRING,
            _ => continue,
        };

        let delta_line = token_line - prev_line;
        let delta_start = if delta_line == 0 {
            token_col - prev_start_char
        } else {
            token_col
        };

        semantic_tokens.push(SemanticToken {
            delta_line: delta_line as u32,
            delta_start: delta_start as u32,
            length: token_len as u32,
            token_type: token_type_index(kind),
            token_modifiers_bitset: 0,
        });

        prev_line = token_line;
        prev_start_char = token_col;

        // Advance by 1 for separating tokens
        byte_offset += 1;
    }

    Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data: semantic_tokens,
    })))
}

}

fn token_type_index(typ: SemanticTokenType) -> u32 {
    match typ.as_str() {
        "keyword" => 0,
        "variable" => 1,
        "type" => 2,
        "string" => 3,
        _ => 0,
    }
}

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt().init();

    let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());

    let (service, socket) = LspService::new(|client| Backend { client, documents: DashMap::new() });
    Server::new(stdin, stdout, socket).serve(service).await;
}
