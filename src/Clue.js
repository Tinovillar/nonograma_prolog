import React from 'react';

function Clue({ clue, isColumn }) {
    return (
        <div className={`flex ${isColumn ? "flex-col" : ''} items-center justify-evenly text-center w-20 h-20 bg-blue-200 rounded-md m-1 border`} >
            {clue.map((num, i) =>
                <div key={i} className='text-lg w-auto h-auto'>
                    {num}
                </div>
            )}
        </div>
    );
}



export default Clue;