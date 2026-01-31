import * as vscode from 'vscode';

export function activate(context: vscode.ExtensionContext) {
    const selector: vscode.DocumentSelector = [{ language: 'shiden', scheme: 'file' }, { pattern: '**/*.sd' } as any];

    const provider: vscode.DocumentFormattingEditProvider = {
        async provideDocumentFormattingEdits(document: vscode.TextDocument, options: vscode.FormattingOptions, token: vscode.CancellationToken): Promise<vscode.TextEdit[]> {
            const fullRange = new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length));
            const formatted = formatSource(document.getText());
            return [vscode.TextEdit.replace(fullRange, formatted)];
        }
    };

    context.subscriptions.push(vscode.languages.registerDocumentFormattingEditProvider(selector, provider));

    context.subscriptions.push(vscode.commands.registerCommand('shiden.formatDocument', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) { return; }
        const doc = editor.document;
        const edits = await provider.provideDocumentFormattingEdits(doc, { insertSpaces: true, tabSize: 4 }, new vscode.CancellationTokenSource().token);
        if (!edits || edits.length === 0) { return; }
        const wsEdit = new vscode.WorkspaceEdit();
        const fullRange = new vscode.Range(doc.positionAt(0), doc.positionAt(doc.getText().length));
        wsEdit.replace(doc.uri, fullRange, edits[0].newText);
        await vscode.workspace.applyEdit(wsEdit);
        await doc.save();
    }));


    context.subscriptions.push(vscode.workspace.onWillSaveTextDocument(async (e) => {
        const cfg = vscode.workspace.getConfiguration('shiden').get('format.onSave', true);
        if (!cfg) { return; }
        const doc = e.document;
        if (!doc.fileName.endsWith('.sd') && doc.languageId !== 'shiden') { return; }
        const edits = await provider.provideDocumentFormattingEdits(doc, { insertSpaces: true, tabSize: 4 }, new vscode.CancellationTokenSource().token);
        if (!edits || edits.length === 0) { return; }
        e.waitUntil(Promise.resolve(edits));
    }));


    const diagCollection = vscode.languages.createDiagnosticCollection('shiden');
    context.subscriptions.push(diagCollection);

    function findUnquotedForCheck(s: string, pat: string): boolean {
        let inQuote = false;
        let quoteChar = '';
        for (let i = 0; i < s.length; i++) {
            const c = s[i];
            if (inQuote) {
                if (c === quoteChar) { inQuote = false; quoteChar = ''; }
                continue;
            }
            if (c === '"' || c === "'") { inQuote = true; quoteChar = c; continue; }
            if (s.startsWith(pat, i)) { return true; }
        }
        return false;
    }

    function computeDiagnosticsFor(doc: vscode.TextDocument): vscode.Diagnostic[] {
        const diags: vscode.Diagnostic[] = [];
        const src = doc.getText().replace(/\r\n?/g, '\n');
        const lines = src.split('\n');
        let blockLevel = 0;
        const stack: string[] = [];

        for (let i = 0; i < lines.length; i++) {
            const raw = lines[i];
            if (raw.trim() === '') { continue; }


            if (raw.includes('\t')) {
                const r = new vscode.Range(new vscode.Position(i, 0), new vscode.Position(i, raw.length));
                const d = new vscode.Diagnostic(r, `line ${i + 1}: tab characters are not allowed; use 4 spaces`, vscode.DiagnosticSeverity.Error);
                d.code = 'tabs';
                diags.push(d);
            }

            const line = raw.replace(/\t/g, '    ');
            const trimmed = line.trimStart();




            const allowedTypeSuffixes = ["u8", "u16", "u32", "u64", "usize", "i8", "i16", "i32", "i64", "f32", "f64", "str", "char", "bool", "array", "unit"];
            const isLet = trimmed.startsWith('let ') || trimmed.startsWith('let\t');
            if (isLet) {
                let ok = false;
                for (const t of allowedTypeSuffixes) { if (line.trimEnd().endsWith('/' + t)) { ok = true; break; } }
                if (!ok) {
                    const r = new vscode.Range(new vscode.Position(i, 0), new vscode.Position(i, line.length));
                    const d = new vscode.Diagnostic(r, `line ${i + 1}: 'let' statements must end with '/<type>' (e.g., '/unit' or '/str')`, vscode.DiagnosticSeverity.Error);
                    d.code = 'missing-let-type';
                    diags.push(d);
                }
            } else {
                let ok = false;
                if (line.trimEnd().endsWith('/')) { ok = true; }
                for (const t of allowedTypeSuffixes) { if (line.trimEnd().endsWith('/' + t)) { ok = true; break; } }
                if (!ok) {
                    const r = new vscode.Range(new vscode.Position(i, 0), new vscode.Position(i, line.length));
                    const d = new vscode.Diagnostic(r, `line ${i + 1}: statements must end with '/' or '/<type>'`, vscode.DiagnosticSeverity.Error);
                    d.code = 'missing-slash';
                    diags.push(d);
                }
            }


            const leadingSpaces = raw.length - raw.trimStart().length;
            if (leadingSpaces % 4 !== 0) {
                const r = new vscode.Range(new vscode.Position(i, 0), new vscode.Position(i, Math.min(leadingSpaces + 1, raw.length)));
                const d = new vscode.Diagnostic(r, `line ${i + 1}: indentation must be a multiple of 4 spaces`, vscode.DiagnosticSeverity.Error);
                d.code = 'indent-multiple';
                diags.push(d);
            }

            const isClosing = trimmed === 'fn/';
            const expectedIndent = isClosing ? (blockLevel === 0 ? 0 : (blockLevel - 1) * 4) : blockLevel * 4;
            if (leadingSpaces !== expectedIndent) {
                const r = new vscode.Range(new vscode.Position(i, 0), new vscode.Position(i, Math.min(leadingSpaces + 1, raw.length)));
                const d = new vscode.Diagnostic(r, `line ${i + 1}: unexpected indentation, expected ${expectedIndent} spaces`, vscode.DiagnosticSeverity.Error);
                d.code = 'unexpected-indent';
                diags.push(d);
            }


            const semiIdx = line.indexOf(';');
            if (semiIdx !== -1) {
                const r = new vscode.Range(new vscode.Position(i, semiIdx), new vscode.Position(i, semiIdx + 1));
                const d = new vscode.Diagnostic(r, `line ${i + 1}: semicolons are not allowed`, vscode.DiagnosticSeverity.Error);
                d.code = 'semicolon';
                diags.push(d);
            }


            if (findUnquotedForCheck(line, '//')) {
                const idx = line.indexOf('//');
                const r = new vscode.Range(new vscode.Position(i, Math.max(0, idx)), new vscode.Position(i, Math.min(line.length, idx + 2)));
                const d = new vscode.Diagnostic(r, `line ${i + 1}: inline comments '//' are not allowed`, vscode.DiagnosticSeverity.Error);
                d.code = 'inline-comment';
                diags.push(d);
            }


            if (isClosing) {
                stack.pop();
                blockLevel = Math.max(0, blockLevel - 1);
            } else if (trimmed === 'fn main/' || trimmed.startsWith('fn new ') || trimmed.startsWith('if ') || trimmed === 'else/') {
                stack.push(trimmed.split(/\s+/)[0] || 'block');
                blockLevel += 1;
            }
        }

        if (blockLevel !== 0) {
            const lastLine = Math.max(0, lines.length - 1);
            const pos = new vscode.Position(lastLine, lines[lastLine].length);
            const d = new vscode.Diagnostic(new vscode.Range(pos, pos), `unclosed block: missing 'fn/'`, vscode.DiagnosticSeverity.Error);
            d.code = 'unclosed-block';
            diags.push(d);
        }

        return diags;
    }

    function refreshDiagnosticsFor(doc: vscode.TextDocument) {
        const enabled = vscode.workspace.getConfiguration('shiden').get('diagnostics.enabled', true);
        if (!enabled) { diagCollection.delete(doc.uri); return; }
        const diags = computeDiagnosticsFor(doc);
        diagCollection.set(doc.uri, diags);
    }


    context.subscriptions.push(vscode.workspace.onDidOpenTextDocument((doc) => {
        if (!doc.fileName.endsWith('.sd') && doc.languageId !== 'shiden') { return; }
        refreshDiagnosticsFor(doc);
    }));


    context.subscriptions.push(vscode.workspace.onDidSaveTextDocument((doc) => {
        const onSave = vscode.workspace.getConfiguration('shiden').get('diagnostics.onSave', true);
        if (!onSave) { return; }
        if (!doc.fileName.endsWith('.sd') && doc.languageId !== 'shiden') { return; }

        const useCli = vscode.workspace.getConfiguration('shiden').get('diagnostics.useCli', false);
        if (useCli) { runShidenCheckFor(doc); } else { refreshDiagnosticsFor(doc); }
    }));

    async function runShidenCheckFor(doc: vscode.TextDocument) {
        const enabled = vscode.workspace.getConfiguration('shiden').get('diagnostics.enabled', true);
        if (!enabled) { diagCollection.delete(doc.uri); return; }
        const filePath = doc.fileName;
        const workspaceFolder = (vscode.workspace.workspaceFolders && vscode.workspace.workspaceFolders[0]) ? vscode.workspace.workspaceFolders[0].uri.fsPath : undefined;
        const cp = require('child_process');
        try {
            const cmd = `shiden check --format json "${filePath}"`;
            cp.exec(cmd, { cwd: workspaceFolder, timeout: 5000 }, (err: any, stdout: string, stderr: string) => {

                const output = (stderr && stderr.trim() !== '') ? stderr : stdout || (err ? String(err) : '');
                if (!err && (!output || output.trim() === '')) {

                    diagCollection.delete(doc.uri);
                    return;
                }

                let parsed = null;
                try { parsed = JSON.parse(output); } catch (e) {

                    const idx = (typeof output === 'string') ? output.indexOf('{') : -1;
                    if (idx !== -1) {
                        try { parsed = JSON.parse(output.slice(idx)); } catch (e2) { parsed = null; }
                    }
                }
                if (parsed && Array.isArray(parsed.diagnostics)) {

                    const byFile = new Map<string, vscode.Diagnostic[]>();
                    for (const d of parsed.diagnostics) {
                        const msg = d.message || JSON.stringify(d);
                        const line = (typeof d.line === 'number' && !isNaN(d.line)) ? Math.max(0, d.line - 1) : 0;
                        const col = (typeof d.col === 'number' && !isNaN(d.col)) ? Math.max(0, d.col - 1) : 0;
                        const r = new vscode.Range(new vscode.Position(line, col), new vscode.Position(line, col + 1));
                        const sev = (d.severity || 'error').toLowerCase();
                        const severity = sev === 'warning' ? vscode.DiagnosticSeverity.Warning : (sev === 'info' ? vscode.DiagnosticSeverity.Information : vscode.DiagnosticSeverity.Error);
                        const diag = new vscode.Diagnostic(r, msg, severity);
                        diag.source = 'shiden-cli';
                        const fileKey = (d.file && typeof d.file === 'string' && d.file.trim() !== '') ? d.file : filePath;
                        if (!byFile.has(fileKey)) { byFile.set(fileKey, []); }
                        byFile.get(fileKey)!.push(diag);
                    }

                    for (const [fpath, ds] of byFile.entries()) {
                        try {
                            const uri = vscode.Uri.file(fpath);
                            diagCollection.set(uri, ds);
                        } catch (e) {

                            diagCollection.set(doc.uri, ds);
                        }
                    }
                    return;
                }

                const lines = output.toString().split(/\r?\n/).filter((l: string) => l.trim() !== '');
                const diags: vscode.Diagnostic[] = [];
                for (const l of lines) {
                    const message = l.trim();
                    const r = new vscode.Range(new vscode.Position(0, 0), new vscode.Position(0, 1));
                    const d = new vscode.Diagnostic(r, `shiden check: ${message}`, vscode.DiagnosticSeverity.Error);
                    d.source = 'shiden-cli';
                    diags.push(d);
                }
                if (diags.length > 0) { diagCollection.set(doc.uri, diags); } else { diagCollection.delete(doc.uri); }
            });
        } catch (e) {
            refreshDiagnosticsFor(doc);
        }
    }


    context.subscriptions.push(vscode.commands.registerCommand('shiden.runCheck', async (uri?: vscode.Uri) => {
        let doc: vscode.TextDocument | undefined;
        if (uri) {
            doc = await vscode.workspace.openTextDocument(uri);
        } else if (vscode.window.activeTextEditor) {
            doc = vscode.window.activeTextEditor.document;
        }
        if (doc && (doc.fileName.endsWith('.sd') || doc.languageId === 'shiden')) { runShidenCheckFor(doc); }
    }));


    for (const doc of vscode.workspace.textDocuments) {
        if (!doc.fileName.endsWith('.sd') && doc.languageId !== 'shiden') { continue; }

        const useCli = vscode.workspace.getConfiguration('shiden').get('diagnostics.useCli', false);
        if (useCli) { runShidenCheckFor(doc); } else { refreshDiagnosticsFor(doc); }
    }


    context.subscriptions.push(vscode.workspace.onDidChangeConfiguration((e) => {
        if (e.affectsConfiguration('shiden.diagnostics')) {
            for (const doc of vscode.workspace.textDocuments) {
                if (!doc.fileName.endsWith('.sd') && doc.languageId !== 'shiden') { continue; }
                refreshDiagnosticsFor(doc);
            }
        }

        if (e.affectsConfiguration('shiden.lint')) {
            for (const doc of vscode.workspace.textDocuments) {
                if (!doc.fileName.endsWith('.sd') && doc.languageId !== 'shiden') { continue; }
                refreshLintFor(doc);
            }
        }
    }));


    const lintCollection = vscode.languages.createDiagnosticCollection('shiden-lint');
    context.subscriptions.push(lintCollection);

    function computeLintsFor(doc: vscode.TextDocument): vscode.Diagnostic[] {
        const diags: vscode.Diagnostic[] = [];
        const src = doc.getText().replace(/\r\n?/g, '\n');
        const lines = src.split('\n');


        const fnMap = new Map<string, number>();
        const fnLines: Array<{ name: string, line: number }> = [];
        const fnRegex = /^\s*fn\s+new\s+([A-Za-z_][A-Za-z0-9_]*)\b/;
        for (let i = 0; i < lines.length; i++) {
            const m = fnRegex.exec(lines[i]);
            if (m) {
                const name = m[1];
                fnLines.push({ name, line: i });
                if (!fnMap.has(name)) { fnMap.set(name, i); }
            }
        }


        if (!fnMap.has('main')) {
            const pos = new vscode.Position(0, 0);
            const d = new vscode.Diagnostic(new vscode.Range(pos, pos), `missing 'main' function`, vscode.DiagnosticSeverity.Error);
            d.code = 'lint-missing-main';
            diags.push(d);
        }


        const seen = new Map<string, number>();
        for (const f of fnLines) {
            if (seen.has(f.name)) {
                const prevLine = seen.get(f.name)!;
                const r1 = new vscode.Range(new vscode.Position(prevLine, 0), new vscode.Position(prevLine, lines[prevLine].length));
                const r2 = new vscode.Range(new vscode.Position(f.line, 0), new vscode.Position(f.line, lines[f.line].length));
                const d1 = new vscode.Diagnostic(r1, `duplicate function name '${f.name}'`, vscode.DiagnosticSeverity.Warning);
                d1.code = 'lint-duplicate-fn';
                const d2 = new vscode.Diagnostic(r2, `duplicate function name '${f.name}'`, vscode.DiagnosticSeverity.Warning);
                d2.code = 'lint-duplicate-fn';
                diags.push(d1, d2);
            } else {
                seen.set(f.name, f.line);
            }
        }


        for (const f of fnLines) {
            if (f.name === 'main') { continue; }
            const callRegex = new RegExp(`\\b${f.name}\\s*\\(`, 'g');
            let used = false;
            for (let i = 0; i < lines.length; i++) {
                if (i === f.line) { continue; }
                if (callRegex.test(lines[i])) { used = true; break; }
            }
            if (!used) {
                const r = new vscode.Range(new vscode.Position(f.line, 0), new vscode.Position(f.line, lines[f.line].length));
                const d = new vscode.Diagnostic(r, `function '${f.name}' is never called`, vscode.DiagnosticSeverity.Information);
                d.code = 'lint-unused-fn';
                diags.push(d);
            }
        }

        return diags;
    }


    function collectFunctionSignatures(doc: vscode.TextDocument): Map<string, { params: string[], ret?: string, line: number }> {
        const out = new Map<string, { params: string[], ret?: string, line: number }>();
        const rx = /^\s*fn\s+new\s+([A-Za-z_][A-Za-z0-9_]*)(?:\(([^)]*)\))?\/?(?:\s*\/([A-Za-z_][A-Za-z0-9_]*))?/;
        for (let i = 0; i < doc.lineCount; i++) {
            const line = doc.lineAt(i).text;
            const m = rx.exec(line);
            if (m) {
                const name = m[1];
                const rawParams = (m[2] || '').trim();
                const params = rawParams === '' ? [] : rawParams.split(',').map(s => s.trim());
                const ret = m[3];
                out.set(name, { params, ret, line: i });
            }
        }
        return out;
    }

    context.subscriptions.push(vscode.languages.registerHoverProvider(selector, {
        provideHover(document: vscode.TextDocument, position: vscode.Position) {
            const wordRange = document.getWordRangeAtPosition(position, /[A-Za-z_][A-Za-z0-9_]*/);
            if (!wordRange) { return null; }
            const name = document.getText(wordRange);
            const map = collectFunctionSignatures(document);
            const sig = map.get(name);
            if (!sig) { return null; }
            const params = sig.params.join(', ');
            const ret = sig.ret ? ` -> ${sig.ret}` : '';
            const md = new vscode.MarkdownString(`**fn ${name}(${params})${ret}**`);
            md.isTrusted = false;
            return new vscode.Hover(md, wordRange);
        }
    }));

    context.subscriptions.push(vscode.languages.registerSignatureHelpProvider(selector, {
        provideSignatureHelp(document: vscode.TextDocument, position: vscode.Position) {
            const line = document.lineAt(position.line).text.slice(0, position.character);
            const callMatch = /([A-Za-z_][A-Za-z0-9_]*)\s*\([^\)]*$/.exec(line);
            if (!callMatch) { return null; }
            const name = callMatch[1];
            const map = collectFunctionSignatures(document);
            const sig = map.get(name);
            if (!sig) { return null; }
            const params = sig.params.map(p => new vscode.ParameterInformation(p));
            const s = new vscode.SignatureInformation(`${name}(${sig.params.join(', ')})${sig.ret ? ` -> ${sig.ret}` : ''}`);
            s.parameters = params;
            const h = new vscode.SignatureHelp();
            h.signatures = [s];
            h.activeSignature = 0;

            const argsSoFar = line.split('(').pop() || '';
            h.activeParameter = Math.max(0, (argsSoFar.match(/,/g) || []).length);
            return h;
        }
    }, '(', ','));

    function refreshLintFor(doc: vscode.TextDocument) {
        const enabled = vscode.workspace.getConfiguration('shiden').get('lint.enabled', true);
        if (!enabled) { lintCollection.delete(doc.uri); return; }
        const diags = computeLintsFor(doc);
        lintCollection.set(doc.uri, diags);
    }


    context.subscriptions.push(vscode.workspace.onDidOpenTextDocument((doc) => {
        if (!doc.fileName.endsWith('.sd') && doc.languageId !== 'shiden') { return; }
        refreshLintFor(doc);
    }));


    context.subscriptions.push(vscode.workspace.onDidSaveTextDocument((doc) => {
        const onSave = vscode.workspace.getConfiguration('shiden').get('lint.onSave', true);
        if (!onSave) { return; }
        if (!doc.fileName.endsWith('.sd') && doc.languageId !== 'shiden') { return; }
        refreshLintFor(doc);
    }));


    for (const doc of vscode.workspace.textDocuments) {
        if (!doc.fileName.endsWith('.sd') && doc.languageId !== 'shiden') { continue; }
        refreshLintFor(doc);
    }


    context.subscriptions.push(vscode.languages.registerCodeActionsProvider(selector, {
        provideCodeActions(document: vscode.TextDocument, _range: vscode.Range, context: vscode.CodeActionContext): vscode.CodeAction[] {
            const actions: vscode.CodeAction[] = [];

            for (const diag of context.diagnostics) {
                const line = diag.range.start.line;
                const original = document.lineAt(line).text;

                if (diag.code === 'tabs') {
                    const fixed = original.replace(/\t/g, '    ');
                    const action = new vscode.CodeAction('Replace tabs with spaces', vscode.CodeActionKind.QuickFix);
                    const edit = new vscode.WorkspaceEdit();
                    edit.replace(document.uri, document.lineAt(line).range, fixed);
                    action.edit = edit;
                    action.diagnostics = [diag];
                    action.isPreferred = true;
                    actions.push(action);
                    continue;
                }

                if (diag.code === 'missing-slash') {
                    if (!original.trimEnd().endsWith('/')) {
                        const action = new vscode.CodeAction('Append trailing /', vscode.CodeActionKind.QuickFix);
                        const edit = new vscode.WorkspaceEdit();
                        edit.insert(document.uri, document.lineAt(line).range.end, '/');
                        action.edit = edit;
                        action.diagnostics = [diag];
                        action.isPreferred = true;
                        actions.push(action);
                    }
                    continue;
                }

                if (diag.code === 'missing-let-type') {

                    const suffixes = ["unit", "str", "i64", "i32", "f64", "bool", "char"];
                    for (const sfx of suffixes) {
                        const title = `Append '/${sfx}'`;
                        const action = new vscode.CodeAction(title, vscode.CodeActionKind.QuickFix);
                        const edit = new vscode.WorkspaceEdit();
                        if (original.trimEnd().endsWith('/')) {
                            const before = original.replace(/[ \t]*$/u, '');
                            const replaced = before.replace(/\/$/, `/${sfx}`);
                            edit.replace(document.uri, document.lineAt(line).range, replaced);
                        } else {
                            edit.insert(document.uri, document.lineAt(line).range.end, `/${sfx}`);
                        }
                        action.edit = edit;
                        action.diagnostics = [diag];
                        if (sfx === 'unit') { action.isPreferred = true; }
                        actions.push(action);
                    }
                    continue;
                }

                if (diag.code === 'semicolon') {
                    const fixed = original.replace(/;/g, '');
                    const action = new vscode.CodeAction('Remove semicolons', vscode.CodeActionKind.QuickFix);
                    const edit = new vscode.WorkspaceEdit();
                    edit.replace(document.uri, document.lineAt(line).range, fixed);
                    action.edit = edit;
                    action.diagnostics = [diag];
                    actions.push(action);
                    continue;
                }

                if (diag.code === 'inline-comment') {
                    const idx = original.indexOf('//');
                    if (idx !== -1) {
                        let trimmed = original.slice(0, idx).trimEnd();
                        const isLetLine = trimmed.startsWith('let ') || trimmed.startsWith('let\t');
                        if (isLetLine) {
                            if (trimmed.endsWith('/')) { trimmed = trimmed.replace(/\/$/, '/unit'); } else { trimmed = trimmed + '/unit'; }
                        } else {
                            if (!trimmed.endsWith('/')) trimmed += '/';
                        }
                        const action = new vscode.CodeAction('Remove inline comment', vscode.CodeActionKind.QuickFix);
                        const edit = new vscode.WorkspaceEdit();
                        edit.replace(document.uri, document.lineAt(line).range, trimmed);
                        action.edit = edit;
                        action.diagnostics = [diag];
                        actions.push(action);
                    }
                    continue;
                }

                if (diag.code === 'unexpected-indent') {
                    const m = /expected (\d+) spaces/.exec(diag.message);
                    if (m) {
                        const expected = parseInt(m[1], 10);
                        const content = original.trimStart();
                        const fixed = ' '.repeat(expected) + content;
                        const action = new vscode.CodeAction(`Fix indentation to ${expected} spaces`, vscode.CodeActionKind.QuickFix);
                        const edit = new vscode.WorkspaceEdit();
                        edit.replace(document.uri, document.lineAt(line).range, fixed);
                        action.edit = edit;
                        action.diagnostics = [diag];
                        actions.push(action);
                    }
                    continue;
                }

                if (diag.code === 'unclosed-block') {
                    const action = new vscode.CodeAction("Close unclosed block with fn/", vscode.CodeActionKind.QuickFix);
                    const edit = new vscode.WorkspaceEdit();
                    const last = document.lineCount > 0 ? document.lineAt(document.lineCount - 1).range.end : new vscode.Position(0, 0);
                    edit.insert(document.uri, last, '\nfn/\n');
                    action.edit = edit;
                    action.diagnostics = [diag];
                    actions.push(action);
                    continue;
                }
            }
            return actions;
        }
    }, { providedCodeActionKinds: [vscode.CodeActionKind.QuickFix] }));


    const shidenKeywords = [
        'fn', 'new', 'let', 'mut', 'return', 'if', 'else', 'while', 'for', 'in', 'break', 'continue', 'true', 'false'
    ];
    const shidenBuiltins = ['println', 'print', 'push', 'pop', 'len', 'range', 'read_file', 'write_file'];
    const typeSuffixes = ["unit", "i64", "i32", "f64", "str", "char", "bool", "array", "usize", "u8", "u16", "u32", "u64", "f32", "i8", "i16"];

    function collectLocalSymbols(document: vscode.TextDocument, position: vscode.Position): string[] {

        for (let lineNo = position.line; lineNo >= 0; lineNo--) {
            const line = document.lineAt(lineNo).text;
            const m = /^\s*fn\s+new\s+([A-Za-z_][A-Za-z0-9_]*)(?:\(([^)]*)\))?/.exec(line);
            if (m) {
                const paramsRaw = (m[2] || '').trim();
                const params = paramsRaw === '' ? [] : paramsRaw.split(',').map(s => s.trim().split('/')[0].trim());

                const locals = new Set<string>();
                for (let ln = lineNo + 1; ln <= position.line; ln++) {
                    const l = document.lineAt(ln).text;
                    const mm = /^\s*let\s+(?:mut\s+)?([A-Za-z_][A-Za-z0-9_]*)\b/.exec(l);
                    if (mm) { locals.add(mm[1]); }
                }

                for (const p of params) { if (p) { locals.add(p.split(':')[0].trim()); } }
                return Array.from(locals);
            }
        }
        return [];
    }

    context.subscriptions.push(vscode.languages.registerCompletionItemProvider(selector, {
        provideCompletionItems(document: vscode.TextDocument, position: vscode.Position) {
            const suggestions: vscode.CompletionItem[] = [];
            const line = document.lineAt(position.line).text;
            const pre = line.slice(0, position.character);


            if (pre.endsWith('/')) {
                for (const t of typeSuffixes) {
                    const it = new vscode.CompletionItem(t, vscode.CompletionItemKind.TypeParameter);
                    it.detail = 'type suffix';
                    suggestions.push(it);
                }
                return suggestions;
            }


            const locals = collectLocalSymbols(document, position);
            for (const v of locals) {
                const it = new vscode.CompletionItem(v, vscode.CompletionItemKind.Variable);
                it.sortText = 'a';
                suggestions.push(it);
            }


            const sigs = collectFunctionSignatures(document);
            for (const [name, info] of sigs.entries()) {
                const it = new vscode.CompletionItem(name, vscode.CompletionItemKind.Function);
                it.detail = `fn ${name}(${info.params.join(', ')})${info.ret ? ` -> ${info.ret}` : ''}`;
                it.insertText = new vscode.SnippetString(name + '($0)');
                it.sortText = 'm';
                suggestions.push(it);
            }


            for (const b of shidenBuiltins) {
                const it = new vscode.CompletionItem(b, vscode.CompletionItemKind.Function);
                it.detail = 'builtin';
                it.sortText = 'z';
                suggestions.push(it);
            }


            for (const k of shidenKeywords) {
                const it = new vscode.CompletionItem(k, vscode.CompletionItemKind.Keyword);
                it.sortText = 'y';
                suggestions.push(it);
            }

            return suggestions;
        },
        resolveCompletionItem(item: vscode.CompletionItem) { return item; }
    }, '/', '(', ','));
}

export function deactivate() { }


function formatSource(src: string): string {
    const lines = src.replace(/\r\n?/g, '\n').split('\n');
    let blockLevel = 0;
    const out: string[] = [];

    function findUnquoted(s: string, pat: string): number {
        let inQuote = false;
        let quoteChar = '';
        for (let i = 0; i < s.length; i++) {
            const c = s[i];
            if (inQuote) {
                if (c === quoteChar) { inQuote = false; quoteChar = ''; }
                continue;
            }
            if (c === '"' || c === "'") { inQuote = true; quoteChar = c; continue; }
            if (s.startsWith(pat, i)) { return i; }
        }
        return -1;
    }

    for (let raw of lines) {

        raw = raw.replace(/\t/g, '    ');
        const trimmedEnd = raw.replace(/[ \t]+$/u, '');
        if (trimmedEnd.trim() === '') { out.push(''); continue; }


        const idx = findUnquoted(trimmedEnd, '//');
        let line = (idx >= 0) ? trimmedEnd.slice(0, idx) : trimmedEnd;


        line = line.replace(/;/g, '');


        const allowedTypeSuffixes = ["u8", "u16", "u32", "u64", "usize", "i8", "i16", "i32", "i64", "f32", "f64", "str", "char", "bool", "array", "unit"];
        const isLetLine = line.trimStart().startsWith('let ') || line.trimStart().startsWith('let\t');
        if (isLetLine) {
            let ok = false;
            for (const t of allowedTypeSuffixes) { if (line.trimEnd().endsWith('/' + t)) { ok = true; break; } }
            if (!ok) {
                if (line.trimEnd().endsWith('/')) { line = line.trimEnd().replace(/\/$/, '/unit'); } else { line = line.trimEnd() + '/unit'; }
            }
        } else {
            let ok = false;
            if (line.trimEnd().endsWith('/')) { ok = true; }
            for (const t of allowedTypeSuffixes) { if (line.trimEnd().endsWith('/' + t)) { ok = true; break; } }
            if (!ok) { line = line.trimEnd() + '/'; }
        }

        const trimmed = line.trimStart();
        const isClosing = trimmed === 'fn/';


        const expectedIndent = isClosing ? Math.max(0, (blockLevel - 1) * 4) : blockLevel * 4;
        const newLine = ' '.repeat(expectedIndent) + trimmed;
        out.push(newLine);


        if (isClosing) {
            blockLevel = Math.max(0, blockLevel - 1);
        } else if (trimmed === 'fn main/' || trimmed.startsWith('fn new ') || trimmed.startsWith('if ') || trimmed === 'else/') {
            blockLevel += 1;
        }
    }


    return out.join('\n') + '\n';
}