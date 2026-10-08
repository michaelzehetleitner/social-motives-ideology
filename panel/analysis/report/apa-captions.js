(() => {
  "use strict";

  const minorWords = new Set([
    "a", "an", "and", "as", "at", "but", "by", "for", "if", "in",
    "nor", "of", "on", "or", "per", "so", "the", "to", "up", "via",
    "vs", "vs.", "yet"
  ]);

  const significantCase = /[A-Z].*[A-Z]|[a-z].*[A-Z]|[A-Za-z].*\d|\d.*[A-Za-z]/;

  function titleCasePart(part, forceCapital) {
    if (!part || !/[A-Za-zÀ-ÖØ-öø-ÿ]/.test(part)) return part;

    const match = part.match(/^([^A-Za-zÀ-ÖØ-öø-ÿ]*)(.*?)([^A-Za-zÀ-ÖØ-öø-ÿ]*)$/);
    if (!match) return part;
    const [, prefix, core, suffix] = match;

    // Preserve acronyms, mixed-case names, scale codes and statistical labels.
    if (significantCase.test(core)) return part;
    if (!forceCapital && minorWords.has(core.toLowerCase())) {
      return `${prefix}${core.toLowerCase()}${suffix}`;
    }

    const hyphenated = core.split("-").map((piece, index) => {
      if (!piece) return piece;
      if (index > 0 && minorWords.has(piece.toLowerCase())) return piece.toLowerCase();
      return piece.charAt(0).toUpperCase() + piece.slice(1);
    }).join("-");
    return `${prefix}${hyphenated}${suffix}`;
  }

  function apaTitleCase(title) {
    const words = title.trim().split(/\s+/);
    let capitaliseNext = true;
    return words.map((word, index) => {
      const forceCapital = capitaliseNext || index === words.length - 1;
      const converted = titleCasePart(word, forceCapital);
      capitaliseNext = /[:—–]\s*$/.test(word);
      return converted;
    }).join(" ");
  }

  function formatCaption(caption) {
    const original = caption.textContent.trim();
    const match = original.match(/^((?:Table|Figure)[\s\u00a0]+[AS]?\d+)\s*:\s*(.+)$/s);
    if (!match) return;

    const number = document.createElement("span");
    number.className = "apa-caption-number";
    number.textContent = match[1].replace(/\u00a0/g, " ");

    const title = document.createElement("span");
    title.className = "apa-caption-title";
    title.textContent = apaTitleCase(match[2].replace(/\.\s*$/, ""));

    caption.replaceChildren(number, title);
    caption.classList.add("apa-caption");

    // Quarto places ordinary figure captions and custom-float captions below
    // their displays by default. APA places both table and figure titles above.
    const figure = caption.closest("figure");
    if (figure && figure.firstElementChild !== caption) {
      figure.insertBefore(caption, figure.firstElementChild);
    }
  }

  function formatAllCaptions() {
    document.querySelectorAll("figcaption.quarto-float-caption").forEach(formatCaption);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", formatAllCaptions, { once: true });
  } else {
    formatAllCaptions();
  }
})();
