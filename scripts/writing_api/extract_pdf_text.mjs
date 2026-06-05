import fs from "node:fs/promises";
import { PDFParse } from "pdf-parse";

function usage() {
  console.error("Usage: node scripts/writing_api/extract_pdf_text.mjs <pdf-path>");
}

async function main() {
  const pdfPath = process.argv[2];
  if (!pdfPath) {
    usage();
    process.exitCode = 1;
    return;
  }
  const data = await fs.readFile(pdfPath);
  const parser = new PDFParse({ data });
  try {
    const result = await parser.getText();
    process.stdout.write(JSON.stringify({
      pages: (result.pages ?? []).map((page) => ({
        page_number: page.num ?? null,
        text: page.text ?? ""
      }))
    }));
  } finally {
    await parser.destroy();
  }
}

main().catch((error) => {
  console.error(error?.stack ?? error?.message ?? String(error));
  process.exitCode = 1;
});
