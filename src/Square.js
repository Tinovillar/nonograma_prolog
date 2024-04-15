import React from 'react';

function Square({ value, onClick }) {
    return (
        <button className="border bg-white w-10 h-10 rounded" onClick={onClick}>
            {value !== '_' ? value : null}
        </button>
    );
}

export default Square;