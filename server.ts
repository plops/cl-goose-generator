import { execFile } from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';
import express from 'express';

const PORT = 3000;
const ROOT_DIR = process.cwd();
const ASDF_DIR_NAME = 'cl-goose-generator';

async function startServer() {
  const app = express();

  app.get('/api/download-asdf', (_req, res) => {
    const pkgPath = path.join(ROOT_DIR, ASDF_DIR_NAME);
    if (!fs.existsSync(pkgPath)) {
      res.status(404).json({ error: 'ASDF package directory not found.' });
      return;
    }

    execFile(
      'tar',
      ['-czf', '-', '-C', ROOT_DIR, ASDF_DIR_NAME],
      { encoding: 'buffer', maxBuffer: 10 * 1024 * 1024 },
      (err, stdout) => {
        if (err) {
          res.status(500).json({ error: 'Failed to build archive.' });
          return;
        }
        res.setHeader('Content-Type', 'application/gzip');
        res.setHeader(
          'Content-Disposition',
          'attachment; filename="cl-goose-generator.tar.gz"'
        );
        res.send(stdout);
      }
    );
  });

  if (process.env.NODE_ENV === 'production') {
    const distPath = path.join(ROOT_DIR, 'dist');
    app.use(express.static(distPath));
    app.get('/{*splat}', (_req, res) => {
      res.sendFile(path.join(distPath, 'index.html'));
    });
  } else {
    const { createServer: createViteServer } = await import('vite');
    const vite = await createViteServer({
      server: { middlewareMode: true },
      appType: 'spa',
    });
    app.use(vite.middlewares);
  }

  app.listen(PORT, '0.0.0.0', () => {
    console.log(`cl-goose-generator server listening on port ${PORT}`);
  });
}

startServer();
