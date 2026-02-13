const GITHUB_REPO = 'IdiotStudios/Shiden';
const GITHUB_BRANCH = 'main';
const DOCS_PATH = 'docs';

const CATEGORIES = {
  'Getting Started': ['index', 'cli', 'development'],
  'Language': ['syntax', 'format', 'formatting'],
  'Compiler': ['build', 'frontend', 'interpreter'],
  'Libraries': ['libraries-filesystem', 'import-system']
};

const docsCache = {};

function formatTitle(filename) {
  return filename
    .replace(/-/g, ' ')
    .replace(/\b\w/g, c => c.toUpperCase());
}

async function fetchMarkdown(filename) {
  if (docsCache[filename]) {
    return docsCache[filename];
  }

  const url = `https://raw.githubusercontent.com/${GITHUB_REPO}/${GITHUB_BRANCH}/${DOCS_PATH}/${filename}.md`;
  
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Failed to fetch ${filename}: ${response.status}`);
    }
    const markdown = await response.text();
    docsCache[filename] = markdown;
    return markdown;
  } catch (error) {
    console.error('Error fetching markdown:', error);
    throw error;
  }
}

function renderMarkdown(markdown) {
  if (typeof marked === 'undefined') {
    return '<div class="error">Markdown parser not loaded</div>';
  }
  
  marked.setOptions({
    highlight: function(code, lang) {
      return code;
    },
    breaks: true,
    gfm: true
  });
  
  return marked.parse(markdown);
}

function buildSidebar() {
  const nav = document.getElementById('docs-nav');
  if (!nav) return;

  let html = '';
  
  for (const [category, docs] of Object.entries(CATEGORIES)) {
    html += `
      <div class="docs-category">
        <div class="category-title">${category}</div>
        <ul>
    `;
    
    for (const doc of docs) {
      const title = formatTitle(doc);
      html += `
        <li>
          <a href="#${doc}" class="doc-link" data-doc="${doc}">
            ${title}
          </a>
        </li>
      `;
    }
    
    html += `
        </ul>
      </div>
    `;
  }
  
  nav.innerHTML = html;
  
  nav.querySelectorAll('.doc-link').forEach(link => {
    link.addEventListener('click', async (e) => {
      e.preventDefault();
      const docName = link.dataset.doc;
      await loadDocument(docName, true);
      
      nav.querySelectorAll('.doc-link').forEach(l => l.classList.remove('active'));
      link.classList.add('active');
      
      window.location.hash = docName;
    });
  });
}

async function loadDocument(docName, shouldScroll = false) {
  const display = document.getElementById('doc-display');
  if (!display) return;

  display.innerHTML = '<div class="loading">Loading...</div>';

  try {
    const markdown = await fetchMarkdown(docName);
    const html = renderMarkdown(markdown);
    display.innerHTML = html;
    
    if (shouldScroll) {
      window.scrollTo({ top: 0, behavior: 'smooth' });
    }
  } catch (error) {
    display.innerHTML = `
      <div class="error">
        <h2>Error Loading Document</h2>
        <p>Could not load "${docName}.md" from GitHub.</p>
        <p>Error: ${error.message}</p>
      </div>
    `;
  }
}

function setupSearch() {
  const searchInput = document.getElementById('docs-search');
  if (!searchInput) return;

  searchInput.addEventListener('input', (e) => {
    const query = e.target.value.toLowerCase();
    const links = document.querySelectorAll('.doc-link');
    
    links.forEach(link => {
      const text = link.textContent.toLowerCase();
      const parent = link.closest('.docs-category');
      
      if (text.includes(query)) {
        link.style.display = 'block';
      } else {
        link.style.display = 'none';
      }
      
      const visibleLinks = parent.querySelectorAll('.doc-link[style="display: block;"], .doc-link:not([style])');
      if (visibleLinks.length === 0 && query !== '') {
        parent.style.display = 'none';
      } else {
        parent.style.display = 'block';
      }
    });
  });
}

async function init() {
  buildSidebar();
  setupSearch();
  
  const hash = window.location.hash.slice(1);
  const initialDoc = hash || 'index';
  
  await loadDocument(initialDoc, false);
  
  const activeLink = document.querySelector(`[data-doc="${initialDoc}"]`);
  if (activeLink) {
    activeLink.classList.add('active');
  }
  
  window.scrollTo(0, 0);
}

window.addEventListener('hashchange', () => {
  const docName = window.location.hash.slice(1);
  if (docName) {
    loadDocument(docName, true);
    
    document.querySelectorAll('.doc-link').forEach(l => l.classList.remove('active'));
    const activeLink = document.querySelector(`[data-doc="${docName}"]`);
    if (activeLink) {
      activeLink.classList.add('active');
    }
  }
});

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', init);
} else {
  init();
}
