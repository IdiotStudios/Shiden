export function initHeader(root = document) {
  const toggle = root.querySelector('.menu-toggle');
  const links = root.querySelector('.nav-links');
  if (!toggle || !links) return;

  const setExpanded = (v) => toggle.setAttribute('aria-expanded', String(v));

  toggle.addEventListener('click', () => {
    const expanded = toggle.getAttribute('aria-expanded') === 'true';
    setExpanded(!expanded);
    links.classList.toggle('active');
  });

  links.querySelectorAll('a').forEach(a => a.addEventListener('click', () => {
    links.classList.remove('active');
    setExpanded(false);
  }));

  document.addEventListener('click', (e) => {
    if (!e.target.closest('.header')) {
      links.classList.remove('active');
      setExpanded(false);
    }
  });
}

const htmlCache = {};

export async function loadHeader() {
  const container = document.getElementById('site-header');
  if (!container) return;

  if (container.innerHTML.trim() === '') {
    try {
      if (!htmlCache.header) {
        const res = await fetch('/header.html');
        if (!res.ok) throw new Error('Failed to fetch /header.html: ' + res.status);
        htmlCache.header = await res.text();
      }
      container.innerHTML = htmlCache.header;
    } catch (err) {
      console.error('loadHeader error:', err);
      return;
    }
  }

  initHeader(container);
}

export function initFooter(root = document) {
  const year = String(new Date().getFullYear());
  const yearEl = root.querySelector('#footer-year');
  if (yearEl) yearEl.textContent = year;
  const yearLink = yearEl ? yearEl.closest('a') : root.querySelector('.footer-left a');
  if (yearLink) {
    yearLink.href = `https://en.wikipedia.org/wiki/${year}`;
    yearLink.target = '_blank';
    yearLink.rel = 'noopener noreferrer';
    yearLink.setAttribute('aria-label', `Wikipedia page for ${year}`);
  }
}

export async function loadFooter() {
  const container = document.getElementById('site-footer');
  if (!container) return;

  if (container.innerHTML.trim() === '') {
    try {
      if (!htmlCache.footer) {
        const res = await fetch('/footer.html');
        if (!res.ok) throw new Error('Failed to fetch /footer.html: ' + res.status);
        htmlCache.footer = await res.text();
      }
      container.innerHTML = htmlCache.footer;
    } catch (err) {
      console.error('loadFooter error:', err);
      return;
    }
  }

  initFooter(container);
}

Promise.all([loadHeader(), loadFooter()]);